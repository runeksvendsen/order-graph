{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module OrderBook.Graph.Internal.Util
( fromOB
, toSellBuyOrders
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
