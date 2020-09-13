{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Types.Path
( Path'
, Path
, toPath
, toSellPath
, SellPath
, IsPath(..)
)
where

import OrderBook.Graph.Internal.Prelude
import OrderBook.Graph.Types.Currency (Currency)
import OrderBook.Graph.Types.SomeSellOrder (SomeSellOrder'(..), NumType)
import OrderBook.Graph.Exchange (invertSomeSellOrder, rawQty, rawPrice, Order', oQty, oPrice)
import qualified OrderBook.Graph.Build                      as B
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE


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
    , _pStart :: Currency
      -- ^ Start currency
    , _pMoves :: NonEmpty (T.Text, Currency)
      -- ^ One or more moves /to/ the given currency /via/ the given venue.
      -- Last currency is destination currency.
    } deriving (Eq, Show, Generic, Functor)

instance PrettyVal numType => PrettyVal (Path' numType)

type Path = Path' NumType

-- | The same as 'Path', except with different units for price and quantity.
--   Price unit is quantity of /destination currency/ per unit of /start currency/,
--   and quantity unit is /start currency/.
newtype SellPath' numType = SellPath { getPath :: Path' numType }
  deriving (Eq, Generic)

toSellPath
  :: Fractional numType
  => Path' numType
  -> SellPath' numType
toSellPath bp@Path'{..} = SellPath $
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
    (rawQty $ oQty maxOrder)
    (soQuote first)
    (NE.map venueAndBase revSellOrders)
  where
    revSellOrders@(first NE.:| _) = NE.reverse sellOrders
    venueAndBase so = (soVenue so, soBase so)

class IsPath path numType | path -> numType where
    pPrice :: path -> numType
    pQty :: path -> numType
    showPath :: path -> T.Text
    toSellOrder :: path -> SomeSellOrder' numType

instance IsPath (Path' numType) numType where
    pPrice = _pPrice
    pQty = _pQty
    showPath path =
      toS (_pStart path) <> T.concat (NE.toList moves)
      where
        moves = NE.map venueAndBase (_pMoves path)
        venueAndBase :: (T.Text, Currency) -> T.Text
        venueAndBase (venue, base) = " --" <> venue <> "--> " <> toS base
    toSellOrder bp = SomeSellOrder'
      { soPrice = pPrice bp
      , soQty   = pQty bp
      , soBase  = snd $ NE.last (_pMoves bp)
      , soQuote = _pStart bp
      , soVenue = showPath bp
      }

instance IsPath SellPath Rational where
    pPrice = _pPrice . getPath
    pQty = _pQty . getPath
    showPath = showPath . getPath
    toSellOrder = invertSomeSellOrder . toSellOrder . getPath
