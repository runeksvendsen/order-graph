{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Property.Spec
( spec )
where

import           OrderBook.Graph.Internal.Prelude           hiding (NonEmpty)
import qualified OrderBook.Graph.Internal.Util              as Util
import           Common.Util                                (assertMatchedOrders)
import           Property.Orphans                           (NonEmpty(..))
import qualified OrderBook.Graph                            as Lib

import qualified OrderBook.Types                            as OB

import           Test.Hspec
import qualified Test.Hspec.SmallCheck                      as SC
import qualified Control.Category                           as Cat
import qualified Test.SmallCheck.Series                     as Series


spec :: Spec
spec = parallel $ do
    combineSpec
    matchSpec

matchSpec :: Spec
matchSpec = parallel $ do
    describe "single orderbook" $ do
        it "BUY: outputs sell orders sorted by price" $
            SC.property singleOrderbookBuy
        it "SELL: outputs buy orders sorted by price" $
            SC.property singleOrderbookSell
    describe "dual orderbook" $ do
        it "BUY: outputs sell orders, from composed order book, sorted by price" $
            SC.property dualOrderbookBuy
        it "SELL: outputs buy orders, from composed order book, sorted by price" $
            SC.property dualOrderbookSell

combineSpec :: Spec
combineSpec = do
    describe "combine" $ do
        it "(const $ const Nothing) = id" $
            SC.property $ \lst ->
                Util.combine (\_ _ -> Nothing) lst `shouldBe` (lst :: [Int])
        it "(const $ const $ Just value) (NonEmpty _) = [value]" $
            let value = 1 :: Int in
                SC.property $ \(Series.NonEmpty lst) ->
                    when (length lst >= 2) $
                        Util.combine (\_ _ -> Just value) lst `shouldBe` [value]

-- |
singleOrderbookBuy
    :: NonEmpty (OB.OrderBook "TestVenue" "A" "B")
    -> Expectation
singleOrderbookBuy (NonEmpty ob) =
    assertMatchedOrders
        (Util.fromOB ob)
        buyOrder
        obSellOrders -- I have "A" and I want "B"
  where
    (obSellOrders, _) = Util.toSellBuyOrders ob
    buyOrder :: Lib.BuyOrder "A" "B"   -- I have "B" and I want "A"
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing

-- |
singleOrderbookSell
    :: NonEmpty (OB.OrderBook "TestVenue" "A" "B")
    -> Expectation
singleOrderbookSell (NonEmpty ob) =
    assertMatchedOrders
        (Util.fromOB ob)
        buyOrder
        obBuyOrders -- I have "B" and I want "A"
  where
    (_, obBuyOrders) = Util.toSellBuyOrders ob
    buyOrder :: Lib.BuyOrder "B" "A"   -- I have "A" and I want "B"
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing

-- |
dualOrderbookBuy
    :: NonEmpty (OB.OrderBook "TestVenue" "A" "B")
    -> NonEmpty (OB.OrderBook "TestVenue" "B" "C")
    -> Expectation
dualOrderbookBuy (NonEmpty obAB) (NonEmpty obBC) =
    assertMatchedOrders
        (Util.fromOB obAB ++ Util.fromOB obBC)
        buyOrder
        obSellOrders -- I have "A" and I want "C"
  where
    composedOB = obBC Cat.. obAB
    (obSellOrders', _) = Util.toSellBuyOrders composedOB
    -- "ob1 Cat.. ob2" ignores the venue, so we set this to what "Lib.match" produces
    obSellOrders = map (\so -> so { Lib.soVenue = "TestVenue,TestVenue" }) obSellOrders'
    buyOrder :: Lib.BuyOrder "A" "C"   -- I have "C" and I want "A"
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing

-- |
dualOrderbookSell
    :: NonEmpty (OB.OrderBook "TestVenue" "A" "B")
    -> NonEmpty (OB.OrderBook "TestVenue" "B" "C")
    -> Expectation
dualOrderbookSell (NonEmpty obAB) (NonEmpty obBC) =
    assertMatchedOrders
        (Util.fromOB obAB ++ Util.fromOB obBC)
        buyOrder
        obBuyOrders -- I have "C" and I want "A"
  where
    composedOB = obBC Cat.. obAB
    (_, obBuyOrders') = Util.toSellBuyOrders composedOB
    -- "ob1 Cat.. ob2" ignores the venue, so we set this to what "Lib.match" produces
    obBuyOrders = map (\so -> so { Lib.soVenue = "TestVenue,TestVenue" }) obBuyOrders'
    buyOrder :: Lib.BuyOrder "C" "A"   -- I have "A" and I want "C"
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing
