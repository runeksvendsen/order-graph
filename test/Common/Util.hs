{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.Util
( assertMatchedOrders
, shouldBeIgnoringVenue
, fromOB
, toSellBuyOrders
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Internal.Util              (merge)
import qualified OrderBook.Graph                            as Lib
import           Test.Hspec.Expectations.Pretty

import qualified Control.Monad.ST                           as ST
import qualified System.Random.Shuffle                      as Shuffle

import           OrderBook.Graph.Types.SomeSellOrder        (SomeSellOrder, SomeSellOrder'(..))
import qualified OrderBook.Types                            as OB

import qualified Money
import qualified Data.Text                                  as T
import qualified Data.Vector                                as Vec
import           Data.String                                (fromString)
import           GHC.TypeLits                               (KnownSymbol, symbolVal)
import           Data.Proxy                                 (Proxy(..))


-- | Convert all orders in an orderbook (consisting of both sell orders and buy orders)
--    into a list of sell orders
fromOB
    :: forall venue base quote.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => OB.OrderBook venue base quote
    -> [SomeSellOrder]
fromOB ob =
    sellOrders ++ buyOrders
  where
    (sellOrders, buyOrders) = toSellBuyOrders ob

-- | Convert all orders in an orderbook (consisting of both sell orders and buy orders)
--    into a pair of sell orders, where the first item is sell orders and
toSellBuyOrders
    :: forall venue base quote.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => OB.OrderBook venue base quote
    -> ([SomeSellOrder], [SomeSellOrder])   -- ^ (Sell orders, buy orders)
toSellBuyOrders OB.OrderBook{..} =
    ( map (fromSellOrder venue) (Vec.toList $ OB.sellSide obAsks)
    , map (fromSellOrder venue) (map OB.invert . Vec.toList $ OB.buySide obBids)
    )
  where
    venue = fromString $ symbolVal (Proxy :: Proxy venue)

fromSellOrder
    :: forall base quote.
       (KnownSymbol base, KnownSymbol quote)
    => T.Text                   -- ^ Venue
    -> OB.Order base quote      -- ^ Sell order
    -> SomeSellOrder
fromSellOrder venue OB.Order{..} = SomeSellOrder'
    { soPrice = fromRational $ Money.exchangeRateToRational oPrice
    , soQty   = fromRational $ toRational oQuantity
    , soBase  = fromString $ symbolVal (Proxy :: Proxy base)
    , soQuote = fromString $ symbolVal (Proxy :: Proxy quote)
    , soVenue = venue
    }


-- |
assertMatchedOrders
    :: forall base quote.
       ( KnownSymbol base
       , KnownSymbol quote
       )
    => [Lib.SomeSellOrder]          -- ^ Input sell orders
    -> Lib.BuyOrder base quote      -- ^ Buy order
    -> ([Lib.SomeSellOrder] -> IO ())   -- ^ Receives matched orders
    -> IO ()
assertMatchedOrders sellOrders buyOrder f = void $ do
    shuffledSellOrders <- Shuffle.shuffleM sellOrders
    matchedOrders <- ST.stToIO $ do
        mGraph <- Lib.buildFromOrders shuffledSellOrders
        (buyGraph, _) <- Lib.runArb mGraph $ Lib.arbitrages base >> Lib.arbitrages quote
        buyPaths <- Lib.runMatch buyGraph $ Lib.match buyOrder
        return $ map Lib.toSellOrder buyPaths
    assertAscendingPriceSorted matchedOrders
    f (merge matchedOrders)
  where
    quote = fromString $ symbolVal (Proxy :: Proxy quote)
    base = fromString $ symbolVal (Proxy :: Proxy base)

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
