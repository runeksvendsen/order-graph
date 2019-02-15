{-# LANGUAGE DataKinds #-}
module Property.Spec
( spec )
where

import           OrderBook.Graph.Internal.Util              (fromOB, toSellBuyOrders)
import           Common.Util                                (assertMatchedOrders)
import           Property.Orphans                           ()
import qualified OrderBook.Graph                            as Lib

import qualified OrderBook.Types                            as OB

import           Test.Hspec
import qualified Test.Hspec.SmallCheck                      as SC


spec :: Spec
spec = parallel $ do
    describe "single orderbook" $ do
        it "BUY: outputs sell orders sorted by price" $
            SC.property singleOrderbookBuy
        it "SELL: outputs buy orders sorted by price" $
            SC.property singleOrderbookSell

-- |
singleOrderbookBuy
    :: OB.OrderBook "TestVenue" "A" "B"
    -> Expectation
singleOrderbookBuy ob =
    assertMatchedOrders
        (fromOB ob)
        buyOrder
        obSellOrders -- I have "A" and I want "B"
  where
    (obSellOrders, _) = toSellBuyOrders ob
    buyOrder :: Lib.BuyOrder "A" "B"   -- I have "B" and I want "A"
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing

-- |
singleOrderbookSell
    :: OB.OrderBook "TestVenue" "A" "B"
    -> Expectation
singleOrderbookSell ob =
    assertMatchedOrders
        (fromOB ob)
        buyOrder
        obBuyOrders -- I have "B" and I want "A"
  where
    (_, obBuyOrders) = toSellBuyOrders ob
    buyOrder :: Lib.BuyOrder "B" "A"   -- I have "A" and I want "B"
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing
