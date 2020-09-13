{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Types.Path
( Path'
, Path
, PathDescr
, BuyPath
, SellPath
, toPath
, toBuyPath
, toSellPath
, HasPath(..)
, HasPathQuantity(..)
)
where

import OrderBook.Graph.Internal.Prelude
import OrderBook.Graph.Types.Currency (Currency)
import OrderBook.Graph.Types.SomeSellOrder (SomeSellOrder'(..), NumType)
import OrderBook.Graph.Exchange (invertSomeSellOrder, rawQty, rawPrice, Order', oQty, oPrice)
import qualified OrderBook.Graph.Build                      as B
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE


-- | A description of a path (without any quantities)
data PathDescr = PathDescr
    { _pStart :: Currency
    -- ^ Start currency
    , _pMoves :: (NonEmpty (T.Text, Currency))
    -- ^ Each pair denotes a move /to/ the given currency /via/ the given venue.
     -- Last currency is destination currency.
    } deriving (Eq, Show, Ord, Generic)

instance PrettyVal PathDescr

-- | A path from one currency to another, going through at least a single venue, and zero or more intermediate currency+venue.
--   Examples:
--      * USD --bitfinex--> BTC
--      * BTC --binance--> USDT --bitfinex--> USD
--      * USD --bitfinex--> XTZ --binance--> USDT --bitfinex--> JPY --bitfinex--> BTC
data Path' numType = Path'
    { _pPrice :: numType
      -- ^ Unit: quantity of /start currency/ per unit of /destination currency/
      -- (e.g. /USD per BTC/ for path @USD --venue--> BTC@)
    , _pQty   :: numType
      -- ^ Unit: "destination currency"
      -- (e.g. /BTC/ for path @USD --venue--> BTC@)
    , _pPath :: PathDescr
      -- ^ Actual path
    } deriving (Eq, Show, Generic, Functor)

instance PrettyVal numType => PrettyVal (Path' numType)

type Path = Path' NumType

-- | The same as 'Path''
newtype BuyPath' numType = BuyPath' { getBuyPath :: Path' numType }
  deriving (Eq, Generic)

type BuyPath = BuyPath' NumType

-- | The same as 'Path'', except with different units for price and quantity.
--   Price unit is /destination currency/ per /start currency/;
--   quantity unit is /start currency/.
newtype SellPath' numType = SellPath' { getSellPath :: Path' numType }
  deriving (Eq, Generic)

toBuyPath
    :: Path' numType
    -> BuyPath' numType
toBuyPath = BuyPath'

toSellPath
    :: Fractional numType
    => Path' numType
    -> SellPath' numType
toSellPath bp@Path'{..} = SellPath' $
    bp
      { _pPrice = recip $ _pPrice
      , _pQty   = _pQty * _pPrice
      }

instance PrettyVal numType => PrettyVal (SellPath' numType)

type SellPath = SellPath' NumType

-- |
toPath
    :: Order' numType base quote
    -- ^ The result of
    --   @OrderBook.Graph.Exchange.withSomeSellOrders sp $ \_ orders -> Exchange.maxOrder orders@
    -> NonEmpty B.SomeSellOrder
    -- ^ Same order as the first argument to @f@ in @OrderBook.Graph.Exchange.withSomeSellOrders sp f@
    -> Path' numType
toPath maxOrder sellOrders = Path'
    (rawPrice $ oPrice maxOrder)
    (rawQty $ oQty maxOrder) $
    PathDescr
      (soQuote first)
      (NE.map venueAndBase revSellOrders)
  where
    revSellOrders@(first NE.:| _) = NE.reverse sellOrders
    venueAndBase so = (soVenue so, soBase so)

-- | Describes a path
class HasPath path where
    pathDescr :: path -> PathDescr
    showPath :: path -> T.Text

-- | Describes price and quantity for a path
class HasPath path => HasPathQuantity path numType | path -> numType where
    pPrice :: path -> numType
    pQty :: path -> numType
    toSellOrder :: path -> SomeSellOrder' numType

instance HasPath PathDescr where
    pathDescr = id
    showPath (PathDescr pathStart pathMoves) =
      toS pathStart <> T.concat (NE.toList moves)
      where
        moves = NE.map venueAndBase pathMoves
        venueAndBase :: (T.Text, Currency) -> T.Text
        venueAndBase (venue, base) = " --" <> venue <> "--> " <> toS base

instance HasPath (Path' numType) where
    pathDescr = _pPath
    showPath = showPath . _pPath

instance HasPathQuantity (Path' numType) numType where
    pPrice = _pPrice
    pQty = _pQty
    toSellOrder bp = SomeSellOrder'
      { soPrice = pPrice bp
      , soQty   = pQty bp
      , soBase  = snd $ NE.last (_pMoves $ _pPath bp)
      , soQuote = _pStart $ _pPath bp
      , soVenue = showPath bp
      }

instance HasPath BuyPath where
    pathDescr = pathDescr . getBuyPath
    showPath = showPath . getBuyPath

instance HasPathQuantity BuyPath Rational where
    pPrice = pPrice . getBuyPath
    pQty = pQty . getBuyPath
    toSellOrder = toSellOrder . getBuyPath

instance HasPath SellPath where
    pathDescr = pathDescr . getSellPath
    showPath = showPath . getSellPath

instance HasPathQuantity SellPath Rational where
    pPrice = _pPrice . getSellPath
    pQty = _pQty . getSellPath
    toSellOrder = invertSomeSellOrder . toSellOrder . getSellPath
