{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Unit.Integration
( tests )
where

import qualified OrderBook.Graph as Lib

import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Control.Monad.ST as ST
import           Test.HUnit
import           Test.Hspec.Expectations.Pretty


tests :: Test
tests = TestLabel "match" $ TestList
    [ TestLabel "buy path" matchBuyPath
    , TestLabel "sell path" matchSellPath
    , TestLabel "sell+buy path" matchSellBuyPath
    ]

-- From the POV of the buyer, a buy path consumes sell orders
matchBuyPath :: Test
matchBuyPath =
    TestCase $ ST.runST (matchOrderBooks [orderBook])
        `shouldBe` ([], ["10.0 @ 10000.0" <> path, "20.0 @ 11000.0" <> path])
  where
    orderBook :: Lib.OrderBook Rational
    orderBook = Lib.mkOrderBook bids asks "TestVenue" "BTC" "USD"
    path = " USD --TestVenue--> BTC"
    bids = Vec.fromList []
    asks = Vec.fromList [Lib.mkOrder 10 10000, Lib.mkOrder 20 11000]

-- From the POV of the seller, a sell path consumes buy orders
matchSellPath :: Test
matchSellPath =
    TestCase $ ST.runST (matchOrderBooks [orderBook])
        `shouldBe` (["5.0 @ 9000.0" <> path, "15.0 @ 8500.0" <> path], [])
  where
    orderBook :: Lib.OrderBook Rational
    orderBook = Lib.mkOrderBook bids asks "TestVenue" "BTC" "USD"
    path = " BTC --TestVenue--> USD"
    bids = Vec.fromList [Lib.mkOrder 5 9000, Lib.mkOrder 15 8500]
    asks = Vec.fromList []

-- From the POV of the seller, a sell path consumes buy orders
matchSellBuyPath :: Test
matchSellBuyPath =
    TestCase $ ST.runST (matchOrderBooks [orderBook])
        `shouldBe` ( ["1.0 @ 7000.0" <> pathSell, "2.0 @ 6500.0" <> pathSell]
                   , ["3.0 @ 7500.0" <> pathBuy, "1.5 @ 7700.0" <> pathBuy]
                   )
  where
    orderBook :: Lib.OrderBook Rational
    orderBook = Lib.mkOrderBook bids asks "TestVenue" "BTC" "USD"
    pathSell = " BTC --TestVenue--> USD"
    pathBuy = " USD --TestVenue--> BTC"
    bids = Vec.fromList [Lib.mkOrder 1 7000, Lib.mkOrder 2 6500]
    asks = Vec.fromList [Lib.mkOrder 3 7500, Lib.mkOrder 1.5 7700]

matchOrderBooks
    :: forall numType s. (Real numType, Fractional numType)
    => [Lib.OrderBook numType]
    -> ST.ST s ([T.Text], [T.Text])
matchOrderBooks orderBooks = do
    (sellPath, buyPaths) <- Lib.buildBuyGraph noLogging 1e9 orderBooks >>= Lib.matchOrders noLogging "USD" "BTC" . snd
    return
        ( map (Lib.showPathQty . sellPathDouble) sellPath
        , map (Lib.showPathQty . buyPathDouble) buyPaths
        )
  where
    sellPathDouble :: Lib.SellPath -> Lib.SellPath' Rational
    sellPathDouble = fmap fromRational
    buyPathDouble :: Lib.BuyPath -> Lib.BuyPath' Rational
    buyPathDouble = fmap fromRational
    noLogging :: Monad m => String -> m ()
    noLogging = const $ return ()
