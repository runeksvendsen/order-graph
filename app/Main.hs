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
import           Data.List                                  (sortBy, nub)
import           Data.Ord                                   (comparing)

import qualified Data.Aeson                                 as Json
import           Data.Aeson                                 ((.=))
import           System.FilePath                            ((</>))
import qualified System.FilePath                            as FP
import qualified Criterion
import qualified Criterion.Main                             as Criterion
import qualified Criterion.Main.Options                     as Criterion
import qualified Criterion.Types                            as Criterion
import qualified Data.List.NonEmpty                         as NE


main :: IO ()
main = do
    options <- Opt.execParser Opt.opts
    let executions = NE.map (execution options) (Opt.inputFiles options)
    let execute = case Opt.mode options of
            Opt.Analyze             -> analyze options
            Opt.Visualize outputDir -> visualize options outputDir
            Opt.Benchmark           -> benchmark Nothing
            Opt.BenchmarkCsv csvOut -> benchmark (Just csvOut)
    execute $ NE.toList executions

data Execution = Execution
    { inputFile     :: FilePath
      -- ^ Input order book file
    , preRun        :: IO [SomeSellOrder]
      -- ^ Produce input data
    , mainRun       :: [SomeSellOrder] -> IO ([SomeSellOrder], [SomeSellOrder])
      -- ^ Process input data
    }

execution :: Opt.Options -> FilePath -> Execution
execution options inputFile =
    Execution inputFile (readOrdersFile inputFile) mainRun
  where
    mainRun orders =
        withBidsAsksOrder options $ \bidsOrder asksOrder ->
            matchOrders bidsOrder asksOrder orders

analyze :: Opt.Options -> [Execution] -> IO ()
analyze options executions =
    forM_ executions $ \(Execution inputFile preRun mainRun) -> do
        (buyOrders, sellOrders) <- preRun >>= mainRun
        -- print stuff
        logLine "Order book file" inputFile
        logLine "Market (base/quote)" (ordersMarket buyOrders)
        logLine "Maximum slippage" (show (Opt.maxSlippage options) ++ "%")
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

visualize :: Opt.Options -> FilePath -> [Execution] -> IO ()
visualize options outputDir executions =
    sequence_ $
        map runExecution executions
  where
    mkOutFileName path = FP.takeBaseName path <> "-" <> Opt.crypto options <> FP.takeExtension path
    mkOutFilePath inputFile = outputDir </> mkOutFileName inputFile
    runExecution (Execution inputFile preRun mainRun) =
        preRun >>= mainRun >>= writeChartFile (mkOutFilePath inputFile)

-- |
benchmark
    :: Maybe FilePath   -- ^ Write results to CSV file?
    -> [Execution]
    -> IO ()
benchmark csvFileM executions = do
    benchmarks <- mapM benchExecution executions
    Criterion.runMode mode benchmarks
  where
    mode = Criterion.Run config Criterion.Prefix [""]
    config = Criterion.defaultConfig { Criterion.csvFile = csvFileM }
    benchExecution (Execution inputFile preRun mainRun) = do
        benchSingle inputFile preRun (void . mainRun)

-- |
benchSingle
    :: FilePath                     -- ^ Order book input file name
    -> IO [SomeSellOrder]           -- ^ Read order book from file
    -> ([SomeSellOrder] -> IO ())   -- ^ Run algorithm
    -> IO Criterion.Benchmark
benchSingle obFile readOrders action = do
    sellOrders <- readOrders
    (vertexCount, edgeCount) <- ST.stToIO $ DG.withGraph (buildGraph sellOrders)
    let name = obFile ++ " V=" ++ show vertexCount ++ " E=" ++ show edgeCount
    return $ Criterion.bench name $
        Criterion.perBatchEnv (const readOrders) action

withBidsAsksOrder
    :: Opt.Options
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Lib.BuyOrder dst src
                                                           -> Lib.BuyOrder src dst
                                                           -> r
       )
    -> r
withBidsAsksOrder options f =
    case someSymbolVal (uppercase $ Opt.numeraire options) of
        SomeSymbol (Proxy :: Proxy numeraire) ->
            case someSymbolVal (uppercase $ Opt.crypto options) of
                SomeSymbol (Proxy :: Proxy crypto) ->
                    f (buyOrder :: Lib.BuyOrder numeraire crypto)
                      (buyOrder :: Lib.BuyOrder crypto numeraire)
  where
    buyOrder = Lib.unlimited
        { Lib.boMaxSlippage = Just . fromIntegral . Opt.maxSlippage $ options }

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
    -> m (Word, Word)                               -- ^ (Vertex count, edge count)
buildGraph sellOrders graph = do
    Lib.build graph sellOrders
    vertexCount <- DG.vertexCount graph
    edgeCount <- DG.edgeCount graph
    return (vertexCount, edgeCount)

matchOrders
    :: (KnownSymbol src, KnownSymbol dst)
    => Lib.BuyOrder dst src     -- ^ Sell cryptocurrency for national currency
    -> Lib.BuyOrder src dst     -- ^ Buy cryptocurrency for national currency
    -> [SomeSellOrder]          -- ^ Input orders
    -> IO ([SomeSellOrder], [SomeSellOrder])    -- ^ (bids, asks)
matchOrders bidsOrder asksOrder sellOrders =
    ST.stToIO $ DG.withGraph $ \mGraph -> do
        log "Building graph..."
        (vertexCount, edgeCount) <- buildGraph sellOrders mGraph
        log $ "Vertex count: " ++ show vertexCount
        log $ "Edge count:   " ++ show edgeCount
        -- Arbitrages
        buyGraph <- Lib.runArb mGraph $ do
            log "Finding arbitrages..."
            -- Asks
            (_, arbsA) <- Lib.arbitrages asksOrder
            log $ unlines ["Arbitrages (asks):", pp arbsA]
            -- Bids
            --  If there's no path from "asks" start vertex to its end vertex,
            --   then the below might find additional negative cycles.
            (buyGraph, arbsB) <- Lib.arbitrages bidsOrder
            log $ unlines ["Arbitrages (bids):", pp arbsB]
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
log str = return () -- str `trace` return ()
