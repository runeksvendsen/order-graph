{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main
( main )
where


import           Prelude                                    hiding (log)
import qualified Options                                    as Opt
import           OrderBook.Graph.Internal.Prelude           hiding (log)
import qualified OrderBook.Graph.Internal.Util              as Util
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import           OrderBook.Graph.Types.Book                 (OrderBook)
import qualified OrderBook.Graph.Types.Book                 as Book
import qualified OrderBook.Graph                            as Lib

import qualified Control.Monad.ST                           as ST
import qualified Data.Graph.Digraph                         as DG
import           Data.List                                  (sortBy, nub, (\\))
import           Data.Ord                                   (comparing)

import qualified Data.Aeson                                 as Json
import           Data.Aeson                                 ((.=))
import           System.FilePath                            ((</>))
import qualified System.FilePath                            as FP
import qualified Criterion
import qualified Criterion.Main                             as Criterion
import qualified Criterion.Main.Options                     as Criterion
import qualified Criterion.Types                            as Criterion
import qualified Control.Logging                            as Log
import           System.IO.Unsafe                           (unsafePerformIO)
import qualified UnliftIO.Async                             as Async
import qualified Data.Csv.Incremental                       as Csv
import qualified System.IO                                  as IO


main :: IO ()
main = Opt.withOptions $ \options ->
    Opt.withNumberType options $ \(Opt.SomeNumberType (_ :: Proxy numType)) ->
    forM_ (Opt.inputFiles options) $ \inputFile -> do
        sellOrders :: [OrderBook numType] <- readOrdersFile options inputFile
        graphInfo  <- ST.stToIO $ DG.withGraph (buildGraph sellOrders)
        let executionCryptoList = mkExecutions options graphInfo inputFile
        logResult <- forAll (Opt.mode options) executionCryptoList $ \(execution, crypto) -> do
            case Opt.mode options of
                    Opt.Analyze ->
                        (Just . logLiquidityInfo) <$> analyze (Opt.maxSlippage options) execution
                    Opt.AnalyzeCsv ->
                        (Just . csvLiquidityInfo) <$> analyze (Opt.maxSlippage options) execution
                    Opt.Visualize outputDir -> do
                        visualize crypto outputDir execution
                        return Nothing
                    Opt.Benchmark -> do
                        benchmark Nothing execution
                        return Nothing
                    Opt.BenchmarkCsv csvOut -> do
                        benchmark (Just csvOut) execution
                        return Nothing
        forM_ (catMaybes logResult) putStr

-- Parallelize everything, unless it's related to measuring speed/performance
forAll :: Opt.Mode
          -- ^ Mode
       -> [a]
          -- ^ Input list
       -> (a -> IO (Maybe String))
          -- ^ Do for all list items; return output
       -> IO [Maybe String]
          -- ^ All outputs
forAll  Opt.AnalyzeCsv   =
            let addCsvHeader = fmap (fmap (Just csvHeader :))
            in addCsvHeader . concurrent
forAll  Opt.Analyze         = concurrent
forAll (Opt.Visualize _)    = concurrent
forAll  Opt.Benchmark       = sequential
forAll (Opt.BenchmarkCsv _) = sequential

concurrent :: [a] -> (a -> IO b) -> IO [b]
concurrent = flip Async.pooledMapConcurrently

sequential :: [a] -> (a -> IO b) -> IO [b]
sequential = flip mapM

csvHeader :: String
csvHeader = toS . Csv.encode $ Csv.encodeRecord
    ( "file"
    , "market"
    , "max_slippage"
    , "buy_liquidity"
    , "sell_liquidity"
    , "sum_liquidity"
    )

data Execution numType = Execution
    { inputFile     :: FilePath
      -- ^ Input order book file
    , graphInfo     :: GraphInfo numType
      -- ^ Information about the graph
    , preRun        :: IO [OrderBook numType]
      -- ^ Produce input data
    , mainRun       :: [OrderBook numType] -> IO ([SomeSellOrder], [SomeSellOrder])
      -- ^ Process input data
    }

mkExecutions
    :: (Json.FromJSON numType, Fractional numType, Real numType)
    => Opt.Options
    -> GraphInfo numType
    -> FilePath
    -> [(Execution numType, Lib.Currency)]
mkExecutions options graphInfo inputFile = do
    map (\crypto -> (mkExecution crypto, crypto)) allCryptos
  where
    allCryptos = case Opt.crypto options of
            Opt.Single crypto -> [crypto]
            Opt.AllCryptos    -> giVertices graphInfo \\ [numeraire]
    mkExecution crypto =
        Execution inputFile graphInfo (readOrdersFile options inputFile) (mainRun crypto)
    mainRun crypto orders =
        withBidsAsksOrder numeraire crypto $ \buyOrder sellOrder ->
            matchOrders buyOrder sellOrder orders
    numeraire   = Opt.numeraire options

-- |
data LiquidityInfo = LiquidityInfo
    { liInputFile       :: FilePath
    , liBaseQuote       :: Maybe (Lib.Currency, Lib.Currency)
      -- ^ (base, quote) currency
    , liMaxSlippage     :: Double
    , liBuyLiquidity    :: Lib.NumType
    , liSellLiquidity   :: Lib.NumType
    }

analyze :: Double -> Execution numType -> IO LiquidityInfo
analyze maxSlippage Execution{..} = do
    (buyOrders, sellOrders) <- preRun >>= mainRun
    let sellLiquidity = quoteSum buyOrders
        buyLiquidity = quoteSum sellOrders
    return $ LiquidityInfo
        { liInputFile       = inputFile
        , liBaseQuote       = ordersMarket (buyOrders ++ sellOrders)
        , liMaxSlippage     = maxSlippage
        , liBuyLiquidity    = buyLiquidity
        , liSellLiquidity   = sellLiquidity
        }
  where
    ordersMarket orderList = orderMarket <$> headMay orderList
    orderMarket order = (Lib.soBase order, Lib.soQuote order)
    quoteSum orderList = sum $ map quoteQuantity orderList
    quoteQuantity order = Lib.soQty order * Lib.soPrice order

logLiquidityInfo :: LiquidityInfo -> String
logLiquidityInfo LiquidityInfo{..} = unlines $
    [ logLine "Order book file" liInputFile
    , logLine "Market (base/quote)" $ showBaseQuote liBaseQuote
    , logLine "Maximum slippage" (show liMaxSlippage ++ "%")
    ] ++ case liBaseQuote of
        Nothing -> []
        Just (_, quoteCurrency) ->
            [ logLiquidity "buy liquidity" liBuyLiquidity quoteCurrency
            , logLiquidity "sell liquidity" liSellLiquidity quoteCurrency
            , logLiquidity "SUM" (liBuyLiquidity + liSellLiquidity) quoteCurrency
            ]
      ++
    [ "-----------------------------------------------------"
    ,  ""
    ]
  where
    showBaseQuote = maybe "<no orders>" (\(base, quote) -> show base ++ "/" ++ show quote)
    thousandSeparator numStr = reverse $ foldr (\(index, char) accum -> if index /= 0 && index `mod` 3 == 0 then ',' : char : accum else char : accum) [] (zip [0..] (reverse numStr))
    showAmount :: Lib.NumType -> String
    showAmount = thousandSeparator . show @Integer . floor
    logLiquidity :: String -> Lib.NumType -> Lib.Currency -> String
    logLiquidity liquidityText amount quoteCurrency =
        logLine liquidityText
                (showAmount amount ++ " " ++ toS quoteCurrency)
    logLine :: String -> String -> String
    logLine title message =
            printf "%-25s%s" title message

-- csvLiquidityInfo :: LiquidityInfo -> Csv.Builder

csvLiquidityInfo LiquidityInfo{..} = toS . Csv.encode $
    Csv.encodeRecord
        ( liInputFile
        , showBaseQuote liBaseQuote
        , liMaxSlippage
        , round liBuyLiquidity                     :: Integer
        , round liSellLiquidity                    :: Integer
        , round $ liBuyLiquidity + liSellLiquidity :: Integer
        )
  where
    maybeQuote = fst <$> liBaseQuote
    showBaseQuote = maybe "<no orders>" (\(base, quote) -> show base ++ "/" ++ show quote)

visualize :: Lib.Currency -> FilePath -> Execution numType -> IO ()
visualize currency outputDir Execution{..} =
    preRun >>= mainRun >>= writeChartFile outFilePath
  where
    mkOutFileName path = FP.takeBaseName path <> "-" <> toS currency <> FP.takeExtension path
    outFilePath = outputDir </> mkOutFileName inputFile


-- |
benchmark
    :: NFData numType
    => Maybe FilePath   -- ^ Write results to CSV file?
    -> Execution numType
    -> IO ()
benchmark csvFileM Execution{..} = do
    benchmark' <- benchSingle inputFile graphInfo preRun (void . mainRun)
    Criterion.runMode mode [benchmark']
  where
    mode = Criterion.Run config Criterion.Prefix [""]
    config = Criterion.defaultConfig { Criterion.csvFile = csvFileM }

-- |
benchSingle
    :: NFData numType
    => FilePath                     -- ^ Order book input file name
    -> GraphInfo numType
    -> IO [OrderBook numType]           -- ^ Read order book from file
    -> ([OrderBook numType] -> IO ())   -- ^ Run algorithm
    -> IO Criterion.Benchmark
benchSingle obFile GraphInfo{..} readBooks action = do
    let name = obFile ++ " V=" ++ show (length giVertices) ++ " E=" ++ show giEdgeCount
    return $ Criterion.bench name $
        Criterion.perBatchEnv (const readBooks) action

-- |
withBidsAsksOrder
    :: Lib.Currency -- ^ Numeraire
    -> Lib.Currency -- ^ Cryptocurrency
       -- |
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Lib.BuyOrder dst src
                                                              -- ^ Buy order
                                                           -> Lib.BuyOrder src dst
                                                              -- ^ Sell order
                                                           -> r
       )
    -> r
withBidsAsksOrder numeraire crypto f =
    case someSymbolVal (toS numeraire) of
        SomeSymbol (Proxy :: Proxy numeraire) ->
            case someSymbolVal (toS crypto) of
                SomeSymbol (Proxy :: Proxy crypto) ->
                    f (buyOrder :: Lib.BuyOrder crypto numeraire)
                      (buyOrder :: Lib.BuyOrder numeraire crypto)
  where
    buyOrder = Lib.unlimited

readOrdersFile
    :: (Json.FromJSON numType, Fractional numType, Real numType)
    => Opt.Options
    -> FilePath
    -> IO [OrderBook numType]
readOrdersFile options filePath = do
    log $ "Reading order books from " ++ show filePath ++ "..."
    books <- decodeFileOrFail filePath
    -- Log venues
    log ("Venues:") >> logVenues (nub $ map Book.bookVenue books)
    let orders = concatMap Book.fromOrderBook books
    log $ "Order book count: " ++ show (length books)
    log $ "Order count: " ++ show (length orders)
    return $ map (Book.trimSlippageOB maxSlippage) books
  where
    maxSlippage = toRational $ Opt.maxSlippage options
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail :: Json.FromJSON numType => FilePath -> IO [OrderBook numType]
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file
    logVenues venues = forM_ venues $ \venue -> log ("\t" ++ toS venue)

buildGraph
    :: (PrimMonad m, Real numType)
    => [OrderBook numType]                         -- ^ Sell orders
    -> Lib.SellOrderGraph (PrimState m) g "arb"     -- ^ Empty graph
    -> m (GraphInfo numType)
buildGraph sellOrders graph = do
    Lib.build graph sellOrders
    currencies <- DG.vertexLabels graph
    edgeCount <- DG.edgeCount graph
    return $ GraphInfo
        { giVertices    = currencies
        , giEdgeCount   = edgeCount
        }

-- NB: Phantom 'numType' is number type of input order book
data GraphInfo numType = GraphInfo
    { giVertices    :: [Lib.Currency]
    , giEdgeCount   :: Word
    }

matchOrders
    :: (KnownSymbol src, KnownSymbol dst, Real numType)
    => Lib.BuyOrder dst src     -- ^ Buy cryptocurrency for national currency
    -> Lib.BuyOrder src dst     -- ^ Sell cryptocurrency for national currency
    -> [OrderBook numType]      -- ^ Input orders
    -> IO ([SomeSellOrder], [SomeSellOrder])    -- ^ (bids, asks)
matchOrders buyOrder sellOrder sellOrders =
    ST.stToIO $ DG.withGraph $ \mGraph -> do
        log "Building graph..."
        graphInfo <- buildGraph sellOrders mGraph
        let vertexCount = length (giVertices graphInfo)
            edgeCount = giEdgeCount graphInfo
        log $ "Vertex count: " ++ show vertexCount
        log $ "Edge count:   " ++ show edgeCount
        -- Arbitrages
        buyGraph <- Lib.runArb mGraph $ do
            log "Finding arbitrages..."
            -- Asks
            (buyGraph, arbs) <- Lib.arbitrages sellOrder
            -- Finds all arbitrages (regardless of "src" vertex)
            log $ unlines ["Arbitrages:", pp arbs]
            return buyGraph
        -- Match
        Lib.runMatch buyGraph $ do
            log "Matching sell order..."
            bids <- map Lib.invertSomeSellOrder <$> Lib.match sellOrder
            log "Matching buy order..."
            asks <- Lib.match buyOrder
            return (bids, asks)

writeChartFile
    :: FilePath
    -> ([SomeSellOrder], [SomeSellOrder])
    -> IO ()
writeChartFile obPath (bids, asks) = do
    log "Writing order book.."
    let trimmedAsks = trimOrders $ sortBy (comparing soPrice)        asks
        trimmedBids = trimOrders $ sortBy (flip $ comparing soPrice) bids
    Json.encodeFile obPath (mkJsonOb trimmedBids trimmedAsks)
    putStrLn $ "Wrote " ++ show obPath
  where
    trimOrders :: [SomeSellOrder] -> [SomeSellOrder]
    trimOrders = Util.compress 500 . Util.merge

-- | Write JSON order book
mkJsonOb
    :: [SomeSellOrder]  -- ^ Bids
    -> [SomeSellOrder]  -- ^ Asks
    -> Json.Value
mkJsonOb bids asks =
    Json.object
        [ toS "bids" .= map toJson bids
        , toS "asks" .= map toJson asks
        ]
  where
    toJson :: SomeSellOrder -> Json.Value
    toJson sso = Json.toJSON -- format: ["0.03389994", 34.14155996]
        ( show (realToFrac $ soPrice sso :: Double)
        , realToFrac $ soQty sso :: Double
        )

log :: Monad m => String -> m ()
log = return . unsafePerformIO . Log.loggingLogger Log.LevelInfo (toS "")
