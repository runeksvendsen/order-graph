{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main
( main )
where

import           Prelude
import qualified Options                                    as Opt
import           OrderBook.Graph.Internal.Prelude
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
import           Debug.Trace                                (trace)
import           System.FilePath                            ((</>))
import qualified System.FilePath                            as FP


main :: IO ()
main = do
    options <- Opt.execParser Opt.opts
    forM_ (Opt.inputFiles options) $ \inputFile ->
        withBidsAsksOrder options $ \bidsOrder asksOrder -> do
            orders <- readOrdersFile inputFile
            putStrLn $ "Order count: " ++ show (length orders)
            marketDepthWriteFile
                (mkOutFile options inputFile)
                bidsOrder
                asksOrder
                orders
  where
    mkOutFile options inputFile = Opt.outputDir options </> FP.takeFileName inputFile

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
    return $ concatMap fromABook (books :: [ABook])
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file

marketDepthWriteFile
    :: (KnownSymbol src, KnownSymbol dst)
    => FilePath
    -> Lib.BuyOrder dst src     -- ^ Sell cryptocurrency for national currency
    -> Lib.BuyOrder src dst     -- ^ Buy cryptocurrency for national currency
    -> [SomeSellOrder]
    -> IO ()
marketDepthWriteFile obPath bidsOrder asksOrder sellOrders = do
    (bids, asks) <- ST.stToIO $ DG.withGraph $ \mGraph -> do
        log "Building graph..."
        Lib.build mGraph sellOrders
        DG.vertexCount mGraph >>= \vertexCount -> log $ "Vertex count: " ++ show vertexCount
        DG.edgeCount mGraph >>= \edgeCount -> log $ "Edge count:   " ++ show edgeCount
        log "Finding arbitrages..."
        -- Asks
        (_, arbsA) <- Lib.arbitrages mGraph asksOrder
        log $ unlines ["Arbitrages (asks):", pp arbsA]
        -- Bids
        --  If there's no path from "asks" start vertex to its end vertex,
        --   then the below might find additional negative cycles.
        (buyGraph, arbsB) <- Lib.arbitrages mGraph bidsOrder
        log $ unlines ["Arbitrages (bids):", pp arbsB]
        log "Matching sell orders..."
        asks <- Lib.match buyGraph asksOrder
        log "Matching buy orders..."
        bids <- map Lib.invertSomeSellOrder <$> Lib.match buyGraph bidsOrder
        return (bids, asks)
    log "Writing order book.."
    let trimmedAsks = trimOrders $ sortBy (comparing soPrice)        asks
        trimmedBids = trimOrders $ sortBy (flip $ comparing soPrice) bids
    let jsonOB = Json.object
            [ "bids" .= map toJson trimmedBids
            , "asks" .= map toJson trimmedAsks
            ]
    Json.encodeFile obPath jsonOB
    putStrLn $ "Wrote " ++ show obPath
  where
    log str = str `trace` return ()
    trimOrders :: [SomeSellOrder] -> [SomeSellOrder]
    trimOrders = Util.compress 500 . Util.merge
    toJson :: SomeSellOrder -> Json.Value
    toJson sso = Json.toJSON -- format: ["0.03389994", 34.14155996]
        ( show (realToFrac $ soPrice sso :: Double)
        , realToFrac $ soQty sso :: Double
        )

fromABook :: ABook -> [SomeSellOrder]
fromABook (ABook ob) = Util.fromOB ob
