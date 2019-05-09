{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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


main :: IO ()
main = Opt.withOptions $ \options ->
    forM_ (Opt.inputFiles options) $ \inputFile -> do
        sellOrders <- readOrdersFile inputFile
        graphInfo  <- ST.stToIO $ DG.withGraph (buildGraph sellOrders)
        let executionCryptoList = mkExecutions options graphInfo inputFile
        forM_ executionCryptoList $ \(execution, crypto) -> do
            let execute = case Opt.mode options of
                    Opt.Analyze             -> analyze (Opt.maxSlippage options)
                    Opt.Visualize outputDir -> visualize crypto outputDir
                    Opt.Benchmark           -> benchmark Nothing
                    Opt.BenchmarkCsv csvOut -> benchmark (Just csvOut)
            execute execution


data Execution = Execution
    { inputFile     :: FilePath
      -- ^ Input order book file
    , graphInfo     :: GraphInfo
      -- ^ Information about the graph
    , preRun        :: IO [SomeSellOrder]
      -- ^ Produce input data
    , mainRun       :: [SomeSellOrder] -> IO ([SomeSellOrder], [SomeSellOrder])
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
        Execution inputFile graphInfo (readOrdersFile inputFile) (mainRun crypto)
    mainRun crypto orders =
        withBidsAsksOrder maxSlippage numeraire crypto $ \bidsOrder asksOrder ->
            matchOrders bidsOrder asksOrder orders
    maxSlippage = Opt.maxSlippage options
    numeraire   = Opt.numeraire options

analyze :: Word -> Execution -> IO ()
analyze maxSlippage Execution{..} = do
    (buyOrders, sellOrders) <- preRun >>= mainRun
    -- print stuff
    logLine "Order book file" inputFile
    logLine "Market (base/quote)" (ordersMarket buyOrders)
    logLine "Maximum slippage" (show maxSlippage ++ "%")
    logLiquidity "buy liquidity" buyOrders
    logLiquidity "sell liquidity" sellOrders
    logLiquidity "SUM" (buyOrders ++ sellOrders)
    putStrLn "-----------------------------------------------------"
    putStrLn ""
  where
    thousandSeparator numStr = reverse $ foldr (\(index, char) accum -> if index /= 0 && index `mod` 3 == 0 then ',' : char : accum else char : accum) [] (zip [0..] (reverse numStr))
    showAmount :: Lib.NumType -> String
    showAmount = thousandSeparator . show @Integer . floor
    logLiquidity :: String -> [SomeSellOrder] -> IO ()
    logLiquidity liquidityText orders =
        logLine liquidityText
                (showAmount (fst $ orderInfo orders) ++ " " ++ toS (snd $ orderInfo orders))
    -- Get (liquidity, baseCurrency) for orders
    quoteSum order = Lib.soQty order * Lib.soPrice order
    ordersMarket orderList = fromMaybe "<no orders>" $ orderMarket <$> headMay orderList
    orderMarket order = toS (Lib.soBase order) ++ "/" ++ toS (Lib.soQuote order)
    orderInfo orderList =
        ( sum $ map quoteSum orderList
        , fromMaybe "<no orders>" $ Lib.soQuote <$> headMay orderList
        )
    logLine :: String -> String -> IO ()
    logLine title message =
            putStrLn $ printf "%-25s%s" title message

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
    -> IO [SomeSellOrder]           -- ^ Read order book from file
    -> ([SomeSellOrder] -> IO ())   -- ^ Run algorithm
    -> IO Criterion.Benchmark
benchSingle obFile GraphInfo{..} readOrders action = do
    let name = obFile ++ " V=" ++ show (length giVertices) ++ " E=" ++ show giEdgeCount
    return $ Criterion.bench name $
        Criterion.perBatchEnv (const readOrders) action

-- |
withBidsAsksOrder
    :: Word         -- ^ Maximum slippage in percent
    -> Lib.Currency -- ^ Numeraire
    -> Lib.Currency -- ^ Cryptocurrency
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Lib.BuyOrder dst src
                                                           -> Lib.BuyOrder src dst
                                                           -> r
       )
    -> r
withBidsAsksOrder maxSlippage numeraire crypto f =
    case someSymbolVal (toS numeraire) of
        SomeSymbol (Proxy :: Proxy numeraire) ->
            case someSymbolVal (toS crypto) of
                SomeSymbol (Proxy :: Proxy crypto) ->
                    f (buyOrder :: Lib.BuyOrder numeraire crypto)
                      (buyOrder :: Lib.BuyOrder crypto numeraire)
  where
    buyOrder = Lib.unlimited
        { Lib.boMaxSlippage = Just . fromIntegral $ maxSlippage }

readOrdersFile :: FilePath -> IO [SomeSellOrder]
readOrdersFile filePath = do
    log $ "Reading order books from " ++ show filePath ++ "..."
    books <- decodeFileOrFail filePath
    -- Log venues
    log ("Venues:") >> logVenues (nub $ map Util.getBookVenue books)
    let orders = concatMap fromABook (books :: [ABook])
    log $ "Order count: " ++ show (length orders)
    return orders
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file
    logVenues venues = forM_ venues $ \venue -> log ("\t" ++ toS venue)

buildGraph
    :: PrimMonad m
    => [SomeSellOrder]                              -- ^ Sell orders
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
    -> [SomeSellOrder]          -- ^ Input orders
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
        [ "bids" .= map toJson bids
        , "asks" .= map toJson asks
        ]
  where
    toJson :: SomeSellOrder -> Json.Value
    toJson sso = Json.toJSON -- format: ["0.03389994", 34.14155996]
        ( show (realToFrac $ soPrice sso :: Double)
        , realToFrac $ soQty sso :: Double
        )

fromABook :: ABook -> [SomeSellOrder]
fromABook (ABook ob) = Util.fromOB ob

log :: Monad m => String -> m ()
log = return . unsafePerformIO . Log.loggingLogger Log.LevelInfo ""
