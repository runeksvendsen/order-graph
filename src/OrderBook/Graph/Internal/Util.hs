{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module OrderBook.Graph.Internal.Util
( fromOB
, toSellBuyOrders
, merge
, trimSlippage
) where

import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
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

-- | merge adjacent orders with same price (ignoring venue)
merge
    :: [SomeSellOrder]
    -- ^ List of orders sorted by price ("soBase" and "soQuote" the same for all orders)
    -> [SomeSellOrder]
merge [] = []
merge (firstOrder : remainingOrders) =
    foldr handleOrder [firstOrder] remainingOrders
  where
    mergeOrders order1 order2 = order1 { soQty = soQty order1 + soQty order2 }
    handleOrder _     [] = error "merge bug"
    handleOrder order orderList@(newestOrder : orderListTail) =
        if soPrice order == soPrice newestOrder
            then mergeOrders newestOrder order : orderListTail
            else order : orderList

-- | Remove orders whose price is some specified percentage
--    further away than the first order's price.
--   E.g. "trimSlippage (10%1) sellOrders"
--    will remove the orders in "sellOrders" whose price is more/less than 10%
--    of the first order in "sellOrders"
trimSlippage
    :: Rational
    -- ^ Slippage in percent. E.g. 50%1 = 50%
    -> [SomeSellOrder]
    -- ^ List of orders sorted by price
    -> [SomeSellOrder]
trimSlippage _ [] = []
trimSlippage percentDifference (firstOrder : remainingOrders) =
    let startPrice = soPrice firstOrder
        filterByPricePercentage order =
            abs ((soPrice order - startPrice) / startPrice) <= (percentDifference / 100)
    in firstOrder : filter filterByPricePercentage remainingOrders
