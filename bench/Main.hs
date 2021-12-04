{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Gauge
import qualified OrderBook.Graph as Lib
import qualified Control.Monad.ST as ST


noLogging :: Monad m => String -> m ()
noLogging = const $ return ()

readBooks :: String -> IO [Lib.OrderBook Double]
readBooks file =
    Lib.readOrdersFile noLogging file

buildMatch :: Lib.Currency -> Lib.Currency -> Rational -> [Lib.OrderBook Double] -> ([Lib.SellPath], [Lib.BuyPath])
buildMatch numeraire crypto maxSlippage orderBooks =
    ST.runST $ Lib.buildBuyGraph noLogging maxSlippage orderBooks >>=
        Lib.matchOrders noLogging numeraire crypto . snd

main :: IO ()
main = do
    books <- readBooks "test/data/double/test19.json"
    Gauge.defaultMain [
        Gauge.bgroup "fib"
            [ Gauge.Benchmark "test" $
                Gauge.whnf (buildMatch "BTC" "USD" (toRational maxSlippage)) books
            ]
        ]
  where
    maxSlippage :: Double
    maxSlippage = 0.5