{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Gauge
import qualified OrderBook.Graph as Lib
import qualified Control.Monad.ST as ST


noLogging :: Monad m => String -> m ()
noLogging = const $ return ()

readBooks :: String -> Double -> IO [Lib.OrderBook Double]
readBooks file slippage =
    Lib.readOrdersFile noLogging (toRational slippage) file

buildMatch :: Lib.Currency -> Lib.Currency -> [Lib.OrderBook Double] -> ([Lib.SellPath], [Lib.BuyPath])
buildMatch numeraire crypto orderBooks =
    ST.runST $ Lib.buildBuyGraph noLogging orderBooks >>=
        Lib.matchOrders noLogging numeraire crypto . snd

main :: IO ()
main = do
    books <- readBooks "test/data/double/test19.json" 0.5
    Gauge.defaultMain [
        Gauge.bgroup "fib"
            [ Gauge.Benchmark "test" $
                Gauge.whnf (buildMatch "BTC" "USD") books
            ]
        ]
