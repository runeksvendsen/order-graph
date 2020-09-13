{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Types.Book
( OrderBook
, mkOrderBook
, mkOrder
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

import           Data.List                                  (sortBy, sortOn)
import           Data.Ord                                   (comparing)
import qualified Data.Vector                                as Vec
import qualified Data.Aeson                                 as Json
import qualified Data.Aeson.Types                           as Json
import           Data.Aeson                                 ((.:))
import qualified Data.Text                                  as T


data OrderBook numType = OrderBook
    { bids  :: Vec.Vector (Order numType)
    , asks  :: Vec.Vector (Order numType)
    , venue :: T.Text
    , base  :: Currency
    , quote :: Currency
    } deriving (Functor, Generic)

data Order numType = Order
    { qty   :: numType
    , price :: numType
    } deriving (Functor, Generic)

instance NFData numType => NFData (OrderBook numType)
instance NFData numType => NFData (Order numType)

-- |
mkOrderBook
    :: Vec.Vector (Order numType) -- ^ bids
    -> Vec.Vector (Order numType) -- ^ asks
    -> Text -- ^ venue
    -> Currency -- ^ base
    -> Currency -- ^ quote
    -> OrderBook numType
mkOrderBook = OrderBook

-- |
mkOrder
    :: numType -- ^ quantity
    -> numType -- ^ price
    -> Order numType
mkOrder = Order

-- | Parse 'OrderBook' from JSON and simultaneously sort orders by price.
--   Buy orders: descending, sell orders: ascending.
instance (Json.FromJSON numType, Ord numType) => Json.FromJSON (OrderBook numType) where
    parseJSON json = do
        book <- jsonParseBookRaw json
        return $ book
            { bids = withList (sortOnDescending price) (bids book)
            , asks = withList (sortOnAscending price) (asks book)
            }
      where
        withList f = Vec.fromList . f . Vec.toList
        sortOnDescending f = sortBy (flip $ comparing f)
        sortOnAscending f = sortOn f

-- | Parse 'OrderBook' from JSON without sorting orders by price.
jsonParseBookRaw
    :: Json.FromJSON numType
    => Json.Value
    -> Json.Parser (OrderBook numType)
jsonParseBookRaw =
    Json.withObject "OrderBook" $ \v -> OrderBook
        <$> v .: "bids"
        <*> v .: "asks"
        <*> v .: "venue"
        <*> v .: "base"
        <*> v .: "quote"

instance Json.FromJSON numType => Json.FromJSON (Order numType)

bookVenue :: OrderBook numType -> T.Text
bookVenue = venue

baseQuote :: OrderBook numType -> (T.Text, T.Text)
baseQuote ob = (toS $ base ob, toS $ quote ob)

-- | Convert all orders in an orderbook (consisting of both sell orders and buy orders)
--    into a pair of sell orders, where the first item is sell orders and
toSellBuyOrders
    :: Real numType
    => OrderBook numType
    -> ([SomeSellOrder], [SomeSellOrder])   -- ^ (Sell orders, buy orders)
toSellBuyOrders ob =
    ( map (fromSellOrder venue base quote) (Vec.toList asks)
    , map (fromSellOrder venue quote base . invert) (Vec.toList bids)
    )
  where
    OrderBook{..} = fmap toRational ob
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

fromOrderBook :: Real numType => OrderBook numType -> [SomeSellOrder]
fromOrderBook ob = concat
    [ sellOrders
    , buyOrders
    ]
  where
    (sellOrders, buyOrders) = toSellBuyOrders ob

-- ^ Same as 'trimSlippageGeneric' but do it for an order book
trimSlippageOB
    :: (Fractional numType, Ord numType)
    => Rational
    -- ^ Slippage in percent. E.g. 50%1 = 50%
    -> OrderBook numType
    -> OrderBook numType
trimSlippageOB maxSlippage ob =
    let buySide = bids ob
        sellSide = asks ob
        trimObSide =
            Vec.fromList . trimSlippageGeneric price (fromRational maxSlippage) . Vec.toList
    in ob
        { bids = trimObSide buySide
        , asks = trimObSide sellSide
        }
