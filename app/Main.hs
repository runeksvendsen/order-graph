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


main :: IO ()
main = do
    [fileName] <- getArgs
    readOrdersWriteFile fileName

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
        log "Building graph..."
        Lib.build mGraph sellOrders
        DG.vertexCount mGraph >>= \vertexCount -> log $ "Vertex count: " ++ show vertexCount
        log "Finding arbitrages..."
        -- TODO: also check arbitrages for "bidsOrder" (different 'src' vertex)
        (buyGraph, arbs) <- Lib.arbitrages mGraph asksOrder
        log $ unlines ["Arbitrages:", pp arbs]
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
    slippageCapOrder = Lib.unlimited { Lib.boMaxSlippage = Just 50 }
    asksOrder :: Lib.BuyOrder "BTC" "USD"
    asksOrder = slippageCapOrder
    bidsOrder :: Lib.BuyOrder "USD" "BTC"
    bidsOrder = slippageCapOrder
    toJson :: SomeSellOrder -> Json.Value
    toJson sso = Json.toJSON -- format: ["0.03389994", 34.14155996]
        ( show (realToFrac $ soPrice sso :: Double)
        , realToFrac $ soQty sso :: Double
        )

fromABook :: ABook -> [SomeSellOrder]
fromABook (ABook ob) = Util.fromOB ob
