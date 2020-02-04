{-# LANGUAGE RecordWildCards #-}
module OrderBook.Graph.Types.Book
( OrderBook
, bookVenue
, baseQuote
, fromOrderBook
, trimSlippageOB
, toSellBuyOrders
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types.Currency             (Currency)
import           OrderBook.Graph.Types.SomeSellOrder
import           OrderBook.Graph.Internal.Util              (trimSlippageGeneric)

import qualified Data.Vector                                as Vec
import qualified Data.Aeson                                 as Json
import qualified Data.Text                                  as T


data OrderBook numType = OrderBook
    { bids  :: Vec.Vector (Order numType)
    , asks  :: Vec.Vector (Order numType)
    , venue :: T.Text
    , base  :: Currency
    , quote :: Currency
    } deriving Generic

data Order numType = Order
    { qty   :: numType
    , price :: numType
    } deriving Generic

instance NFData numType => NFData (OrderBook numType)
instance NFData numType => NFData (Order numType)

instance Json.FromJSON numType => Json.FromJSON (OrderBook numType)
instance Json.FromJSON numType => Json.FromJSON (Order numType)

bookVenue :: OrderBook numType -> T.Text
bookVenue = venue

baseQuote :: OrderBook numType -> (T.Text, T.Text)
baseQuote ob = (toS $ base ob, toS $ quote ob)

-- | Convert all orders in an orderbook (consisting of both sell orders and buy orders)
--    into a pair of sell orders, where the first item is sell orders and
toSellBuyOrders
    :: OrderBook Rational
    -> ([SomeSellOrder], [SomeSellOrder])   -- ^ (Sell orders, buy orders)
toSellBuyOrders OrderBook{..} =
    ( map (fromSellOrder venue base quote) (Vec.toList asks)
    , map (fromSellOrder venue quote base . invert) (Vec.toList bids)
    )
  where
    -- invert . invert = id
    invert :: Fractional numType => Order numType -> Order numType
    invert o = Order
        { qty = qty o * price o
        , price = recip (price o)
        }

-- |
fromSellOrder
    :: T.Text           -- ^ Venue
    -> Currency         -- ^ Base
    -> Currency         -- ^ Quote
    -> Order Rational   -- ^ Order
    -> SomeSellOrder
fromSellOrder venue base quote o =
    SomeSellOrder'
        { soPrice = price o
        , soQty   = qty o
        , soBase  = base
        , soQuote = quote
        , soVenue = venue
        }

fromOrderBook :: OrderBook Rational -> [SomeSellOrder]
fromOrderBook ob = concat
    [ sellOrders
    , buyOrders
    ]
  where
    (sellOrders, buyOrders) = toSellBuyOrders ob

-- ^ Same as 'trimSlippageGeneric' but do it for an order book
trimSlippageOB
    :: Rational
    -- ^ Slippage in percent. E.g. 50%1 = 50%
    -> OrderBook Rational
    -> OrderBook Rational
trimSlippageOB maxSlippage ob =
    let buySide = bids ob
        sellSide = asks ob
        trimObSide =
            Vec.fromList . trimSlippageGeneric price maxSlippage . Vec.toList
    in ob
        { bids = trimObSide buySide
        , asks = trimObSide sellSide
        }
