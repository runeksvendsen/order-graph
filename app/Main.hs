{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumDecimals #-}    -- DEBUG
module Main
( main )
where


import           Prelude                                    hiding (log)
import qualified Options                                    as Opt
import           OrderBook.Graph.Internal.Prelude           hiding (log)
import qualified OrderBook.Graph.Internal.Util              as Util
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import qualified OrderBook.Graph                            as Lib

import           CryptoVenues.Types.ABook                   (ABook(ABook))

import qualified Control.Monad.ST                           as ST
import qualified Data.Graph.Digraph                         as DG
import           Data.List                                  (sortBy)
import           Data.Ord                                   (comparing)

import qualified Data.Aeson                                 as Json
import           Data.Aeson                                 ((.=))
import           System.FilePath                            ((</>))
import qualified System.FilePath                            as FP
import qualified Criterion
import qualified Criterion.Main                             as Criterion
import qualified Criterion.Main.Options                     as Criterion
import qualified Data.List.NonEmpty                         as NE


main :: IO ()
main = do
    options <- Opt.execParser Opt.opts
    let executions = NE.map (execution options) (Opt.inputFiles options)
    let execute = case Opt.mode options of
            Opt.Visualize outputDir -> visualize outputDir
            Opt.Benchmark           -> benchmark
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

visualize :: FilePath -> [Execution] -> IO ()
visualize outputDir executions =
    sequence_ $
        map runExecution executions
  where
    mkOutFilename inputFile = outputDir </> FP.takeFileName inputFile
    runExecution (Execution inputFile preRun mainRun) =
        preRun >>= mainRun >>= writeChartFile (mkOutFilename inputFile)

benchmark :: [Execution] -> IO ()
benchmark executions =
    Criterion.runMode mode $
        map benchExecution executions
  where
    mode = Criterion.Run Criterion.defaultConfig Criterion.Prefix [""]
    benchExecution (Execution inputFile preRun mainRun) =
        benchSingle inputFile preRun (void . mainRun)

-- |
benchSingle
    :: FilePath                     -- ^ Order book input file name
    -> IO [SomeSellOrder]           -- ^ Read order book from file
    -> ([SomeSellOrder] -> IO ())   -- ^ Run algorithm
    -> Criterion.Benchmark
benchSingle obFile readOrders action =
    Criterion.bench obFile $
        Criterion.perBatchEnv (const readOrders) action

withBidsAsksOrder
    :: Opt.Options
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Lib.BuyOrder dst src
                                                           -> Lib.BuyOrder src dst
                                                           -> r
       )
    -> r
withBidsAsksOrder options f =
    case someSymbolVal (Opt.numeraire options) of
        SomeSymbol (Proxy :: Proxy numeraire) ->
            case someSymbolVal (Opt.crypto options) of
                SomeSymbol (Proxy :: Proxy crypto) ->
                    f (buyOrder :: Lib.BuyOrder numeraire crypto)
                      (buyOrder :: Lib.BuyOrder crypto numeraire)
  where
    buyOrder = Lib.unlimited
        { Lib.boMaxSlippage = Just . fromIntegral . Opt.maxSlippage $ options }

readOrdersFile :: FilePath -> IO [SomeSellOrder]
readOrdersFile filePath = do
    books <- decodeFileOrFail filePath
    let orders = concatMap fromABook (books :: [ABook])
    log $ "Order count: " ++ show (length orders)
    return orders
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file

matchOrders
    :: (KnownSymbol src, KnownSymbol dst)
    => Lib.BuyOrder dst src     -- ^ Sell cryptocurrency for national currency
    -> Lib.BuyOrder src dst     -- ^ Buy cryptocurrency for national currency
    -> [SomeSellOrder]          -- ^ Input orders
    -> IO ([SomeSellOrder], [SomeSellOrder])    -- ^ (bids, asks)
matchOrders bidsOrder asksOrder sellOrders =
    ST.stToIO $ DG.withGraph $ \mGraph -> do
        log "Building graph..."
        Lib.build mGraph sellOrders
        DG.vertexCount mGraph >>= \vertexCount -> log $ "Vertex count: " ++ show vertexCount
        -- TODO: fix BellmanFord
        -- DG.edgeCount mGraph >>= \edgeCount -> log $ "Edge count:   " ++ show edgeCount
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
