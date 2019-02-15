{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Unit.Spec
( tests )
where

import           OrderBook.Graph.Internal.Prelude
import           Common.Util                        (assertMatchedOrders)

import qualified OrderBook.Graph                    as Lib
import qualified Data.Graph.Immutable               as GI

import           Test.HUnit
import qualified Data.List.NonEmpty                 as NE
import           Test.Hspec.Expectations.Pretty


tests :: Test
tests = TestList
  [ TestLabel "single order"  singleOrder
  ]

singleOrder :: Test
singleOrder =
    TestCase $ assertMatchedOrders [testOrder] buyOrder [testOrder]
  where
    buyOrder :: Lib.BuyOrder "BTC" "USD"
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    testOrder :: Lib.SomeSellOrder
    testOrder = Lib.SomeSellOrder'
        { soPrice = 100
        , soQty   = 20
        , soBase  = "BTC"
        , soQuote = "USD"
        , soVenue = "test"
        }

-- threeOrders :: Test
-- threeOrders =
--     TestCase $ assertMatchedOrders [testOrder] buyOrder [order_btcusd]
--   where
--     buyOrder :: Lib.BuyOrder "BTC" "USD"
--     buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing
--     order_btcusd = Lib.SomeSellOrder'
--         { soPrice = 100
--         , soQty   = 20
--         , soBase  = "BTC"
--         , soQuote = "USD"
--         , soVenue = "test"
--         }
--     order_ethusd = Lib.SomeSellOrder'
--         { soPrice = 30
--         , soQty   = 10
--         , soBase  = "ETH"
--         , soQuote = "USD"
--         , soVenue = "test"
--         }
--     order_ethbtc = Lib.SomeSellOrder'
--         { soPrice = 40
--         , soQty   = 7
--         , soBase  = "ETH"
--         , soQuote = "USD"
--         , soVenue = "test"
--         }
