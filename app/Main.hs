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
import qualified Util
import           OrderBook.Graph.Internal.Prelude           hiding (log)
import qualified OrderBook.Graph.Internal.Util              as Util
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import qualified OrderBook.Graph                            as Lib

import           CryptoVenues.Types.ABook                   (ABook(ABook))

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
    forM_ (Opt.inputFiles options) $ \inputFile -> do
        sellOrders <- readOrdersFile options inputFile
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

data Execution = Execution
    { inputFile     :: FilePath
      -- ^ Input order book file
    , graphInfo     :: GraphInfo
      -- ^ Information about the graph
    , preRun        :: IO [ABook]
      -- ^ Produce input data
    , mainRun       :: [ABook] -> IO ([SomeSellOrder], [SomeSellOrder])
      -- ^ Process input data
    }

mkExecutions :: Opt.Options -> GraphInfo -> FilePath -> [(Execution, Lib.Currency)]
mkExecutions options graphInfo inputFile = do
    map (\crypto -> (mkExecution crypto, crypto)) allCryptos
  where
    allCryptos = case Opt.crypto options of
            Opt.Single crypto -> [crypto]
            Opt.AllCryptos    -> giVertices graphInfo \\ [numeraire]
    mkExecution crypto =
        Execution inputFile graphInfo (readOrdersFile options inputFile) (mainRun crypto)
    mainRun crypto orders =
        withBidsAsksOrder numeraire crypto $ \bidsOrder asksOrder ->
            matchOrders bidsOrder asksOrder orders
    numeraire   = Opt.numeraire options

-- |
data LiquidityInfo = LiquidityInfo
    { liInputFile       :: FilePath
    , liBaseQuote       :: Maybe (Lib.Currency, Lib.Currency)
      -- ^ (base, quote) currency
    , liMaxSlippage     :: Word
    , liBuyLiquidity    :: Lib.NumType
    , liSellLiquidity   :: Lib.NumType
    }

analyze :: Word -> Execution -> IO LiquidityInfo
analyze maxSlippage Execution{..} = do
    (buyOrders, sellOrders) <- preRun >>= mainRun
    let buyLiquidity = quoteSum buyOrders
        sellLiquidity = quoteSum sellOrders
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

visualize :: Lib.Currency -> FilePath -> Execution -> IO ()
visualize currency outputDir Execution{..} =
    preRun >>= mainRun >>= writeChartFile outFilePath
  where
    mkOutFileName path = FP.takeBaseName path <> "-" <> toS currency <> FP.takeExtension path
    outFilePath = outputDir </> mkOutFileName inputFile


-- |
benchmark
    :: Maybe FilePath   -- ^ Write results to CSV file?
    -> Execution
    -> IO ()
benchmark csvFileM Execution{..} = do
    benchmark' <- benchSingle inputFile graphInfo preRun (void . mainRun)
    Criterion.runMode mode [benchmark']
  where
    mode = Criterion.Run config Criterion.Prefix [""]
    config = Criterion.defaultConfig { Criterion.csvFile = csvFileM }

-- |
benchSingle
    :: FilePath                     -- ^ Order book input file name
    -> GraphInfo
    -> IO [ABook]           -- ^ Read order book from file
    -> ([ABook] -> IO ())   -- ^ Run algorithm
    -> IO Criterion.Benchmark
benchSingle obFile GraphInfo{..} readBooks action = do
    let name = obFile ++ " V=" ++ show (length giVertices) ++ " E=" ++ show giEdgeCount
    return $ Criterion.bench name $
        Criterion.perBatchEnv (const readBooks) action

-- |
withBidsAsksOrder
    :: Lib.Currency -- ^ Numeraire
    -> Lib.Currency -- ^ Cryptocurrency
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Lib.BuyOrder dst src
                                                           -> Lib.BuyOrder src dst
                                                           -> r
       )
    -> r
withBidsAsksOrder numeraire crypto f =
    case someSymbolVal (toS numeraire) of
        SomeSymbol (Proxy :: Proxy numeraire) ->
            case someSymbolVal (toS crypto) of
                SomeSymbol (Proxy :: Proxy crypto) ->
                    f (buyOrder :: Lib.BuyOrder numeraire crypto)
                      (buyOrder :: Lib.BuyOrder crypto numeraire)
  where
    buyOrder = Lib.unlimited

readOrdersFile :: Opt.Options -> FilePath -> IO [ABook]
readOrdersFile options filePath = do
    log $ "Reading order books from " ++ show filePath ++ "..."
    books <- decodeFileOrFail filePath
    -- Log venues
    log ("Venues:") >> logVenues (nub $ map Util.getBookVenue books)
    let orders = concatMap Util.fromABook (books :: [ABook])
    log $ "Order book count: " ++ show (length books)
    log $ "Order count: " ++ show (length orders)
    return $ map (Util.trimSlippageOB maxSlippage) books
  where
    maxSlippage = fromIntegral $ Opt.maxSlippage options
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file
    logVenues venues = forM_ venues $ \venue -> log ("\t" ++ toS venue)

buildGraph
    :: PrimMonad m
    => [ABook]                              -- ^ Sell orders
    -> Lib.SellOrderGraph (PrimState m) g "arb"     -- ^ Empty graph
    -> m GraphInfo
buildGraph sellOrders graph = do
    Lib.build graph sellOrders
    currencies <- DG.vertexLabels graph
    edgeCount <- DG.edgeCount graph
    return $ GraphInfo
        { giVertices    = currencies
        , giEdgeCount   = edgeCount
        }

data GraphInfo = GraphInfo
    { giVertices    :: [Lib.Currency]
    , giEdgeCount   :: Word
    }

matchOrders
    :: (KnownSymbol src, KnownSymbol dst)
    => Lib.BuyOrder dst src     -- ^ Sell cryptocurrency for national currency
    -> Lib.BuyOrder src dst     -- ^ Buy cryptocurrency for national currency
    -> [ABook]          -- ^ Input orders
    -> IO ([SomeSellOrder], [SomeSellOrder])    -- ^ (bids, asks)
matchOrders bidsOrder asksOrder sellOrders =
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
            (buyGraph, arbs) <- Lib.arbitrages asksOrder
            -- Finds all arbitrages (regardless of "src" vertex)
            log $ unlines ["Arbitrages:", pp arbs]
            return buyGraph
        -- Match
        Lib.runMatch buyGraph $ do
            log "Matching sell orders..."
            asks <- Lib.match asksOrder
            log "Matching buy orders..."
            bids <- map Lib.invertSomeSellOrder <$> Lib.match bidsOrder
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
