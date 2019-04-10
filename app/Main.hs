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

import           Prelude
import           OrderBook.Graph.Internal.Prelude
import           Protolude                                  ((%))
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
import           System.Environment                         (getArgs)
import           Debug.Trace                                (trace)
-- DEBUG
import Control.Concurrent
import Control.Exception.Base


main :: IO ()
main = do
    [fileName] <- getArgs
    readOrdersWriteFile fileName

main' = debugMain

debugMain = do
    tid <- forkIO $
        readOrdersWriteFile "test/data/10-ob.json"
    threadDelay 300e6
    throwTo tid (ErrorCall "lol")

readOrdersWriteFile :: FilePath -> IO ()
readOrdersWriteFile fileName = do
    orders <- readOrdersFile fileName
    putStrLn $ "Order count: " ++ show (length orders)
    marketDepthWriteFile "/Users/runesvendsen/code/order-graph/web/btcusd.json" orders

readOrdersFile :: FilePath -> IO [SomeSellOrder]
readOrdersFile filePath = do
    books <- decodeFileOrFail filePath
    return $ concatMap fromABook (books :: [ABook])
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file

marketDepthWriteFile
    :: FilePath
    -> [SomeSellOrder]
    -> IO ()
marketDepthWriteFile obPath sellOrders = do
    (bids, asks) <- ST.stToIO $ DG.withGraph $ \mGraph -> do
        -- Build
        log "Building graph..."
        Lib.build mGraph sellOrders
        DG.vertexCount mGraph >>= \vertexCount -> log $ "Vertex count: " ++ show vertexCount
        -- Arbitrages
        buyGraph <- Lib.runArb mGraph $ do
            log "Finding arbitrages..."
            (_, arbsA)        <- Lib.arbitrages asksOrder
            log $ unlines ["Ask arbitrages:", pp arbsA]
            (buyGraph, arbsB) <- Lib.arbitrages bidsOrder
            log $ unlines ["Bids arbitrages:", pp arbsB]
            return buyGraph
        -- Match
        Lib.runMatch buyGraph $ do
            log "Matching sell orders..."
            asks <- Lib.match asksOrder
            log "Matching buy orders..."
            bids <- map Lib.invertSomeSellOrder <$> Lib.match bidsOrder
            return (bids, asks)
    log "Writing order book.."
    let trimmedAsks = trimOrders $ sortBy (comparing soPrice)        asks
        trimmedBids = trimOrders $ sortBy (flip $ comparing soPrice) bids
    Json.encodeFile obPath (mkJsonOb trimmedBids trimmedAsks)
    putStrLn $ "Wrote " ++ show obPath
  where
    log str = str `trace` return ()
    trimOrders :: [SomeSellOrder] -> [SomeSellOrder]
    trimOrders = Util.compress 500 . Util.merge . Util.trimSlippage (50%1)
    asksOrder :: Lib.BuyOrder "BTC" "USD"
    asksOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    bidsOrder :: Lib.BuyOrder "USD" "BTC"
    bidsOrder = Lib.BuyOrder' 1.0 Nothing Nothing

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
