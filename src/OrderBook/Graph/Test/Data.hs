{-# LANGUAGE TemplateHaskell #-}
module OrderBook.Graph.Test.Data
( orderbookList
, regressData
)
where

import qualified OrderBook.Graph as Lib

import qualified Data.Aeson as Json
import Data.FileEmbed (embedFile, embedStringFile)
import qualified Data.ByteString


orderbookList :: [Lib.OrderBook Double]
orderbookList =
    either error id (Json.eitherDecodeStrict' testJson)

regressData :: ([(Double, String)], [(Double, String)])
regressData = read testRegressionData

testJson :: Data.ByteString.ByteString
testJson = $(embedFile "test/data/double/test19.json")

testRegressionData :: String
testRegressionData = $(embedStringFile "test/data/regression/double-test19.txt")
