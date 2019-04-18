{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.Util
( assertMatchedOrders
, shouldBeIgnoringVenue
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Internal.Util              (merge)
import qualified OrderBook.Graph                            as Lib
import qualified Data.Graph.Digraph                         as DG
import           Test.Hspec.Expectations.Pretty

import qualified Control.Monad.ST                           as ST
import qualified System.Random.Shuffle                      as Shuffle


-- |
assertMatchedOrders
    :: ( KnownSymbol base
       , KnownSymbol quote
       )
    => [Lib.SomeSellOrder]          -- ^ Input sell orders
    -> Lib.BuyOrder base quote      -- ^ Buy order
    -> ([Lib.SomeSellOrder] -> IO ())   -- ^ Receives matched orders
    -> IO ()
assertMatchedOrders sellOrders buyOrder f = void $ do
    shuffledSellOrders <- Shuffle.shuffleM sellOrders
    matchedOrders <- ST.stToIO $ DG.withGraph $ \mGraph -> do
        Lib.build mGraph shuffledSellOrders
        (buyGraph, _) <- Lib.runArb mGraph $ Lib.arbitrages buyOrder
        Lib.runMatch buyGraph $ Lib.match buyOrder
    assertAscendingPriceSorted matchedOrders
    f (merge matchedOrders)

assertAscendingPriceSorted
    :: [Lib.SomeSellOrder]
    -> IO ()
assertAscendingPriceSorted [] = return ()
assertAscendingPriceSorted (firstOrder : remainingOrders) =
    void $ foldM adjacentOrdersSorted firstOrder remainingOrders
  where
    adjacentOrdersSorted prevOrder nextOrder =
        if Lib.soPrice prevOrder <= Lib.soPrice nextOrder
            then return nextOrder   -- Everything is ok
            else do
                expectationFailure $ unlines ["Orders not sorted:", pp prevOrder, pp nextOrder]
                -- Line below never reached
                return undefined

-- Compare two SomeSellOrder lists ignoring 'soVenue'
shouldBeIgnoringVenue
    :: [Lib.SomeSellOrder]  -- ^ Actual
    -> [Lib.SomeSellOrder]  -- ^ Expected
    -> IO ()
shouldBeIgnoringVenue actual expected =
    clearVenues actual `shouldBe` clearVenues expected
  where
    clearVenues = map clearVenue
    clearVenue so = so { Lib.soVenue = "" }
