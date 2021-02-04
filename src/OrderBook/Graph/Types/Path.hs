{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Types.Path
( Path'
, Path
, PathDescr
, pStart
, pMoves
, BuyPath
, SellPath
, BuyPath'
, SellPath'
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
import Text.Printf (printf)


-- | A description of a path (without any quantities)
data PathDescr = PathDescr
    { _pStart :: !Currency
    -- ^ Start currency
    , _pMoves :: !(NonEmpty (T.Text, Currency))
    -- ^ Each pair denotes a move /to/ the given currency /via/ the given venue.
     -- Last currency is destination currency.
    } deriving (Eq, Show, Ord, Generic)

pStart :: PathDescr -> Currency
pStart = _pStart

pMoves :: PathDescr -> NonEmpty (Text, Currency)
pMoves = _pMoves

instance PrettyVal PathDescr
instance NFData PathDescr

-- | A path from one currency to another, going through at least a single venue, and zero or more intermediate currency+venue.
--   Examples:
--      * USD --bitfinex--> BTC
--      * BTC --binance--> USDT --bitfinex--> USD
--      * USD --bitfinex--> XTZ --binance--> USDT --bitfinex--> JPY --bitfinex--> BTC
data Path' numType = Path'
    { _pPrice :: !numType
      -- ^ Unit: quantity of /start currency/ per unit of /destination currency/
      -- (e.g. /USD per BTC/ for path @USD --venue--> BTC@)
    , _pQty   :: !numType
      -- ^ Unit: "destination currency"
      -- (e.g. /BTC/ for path @USD --venue--> BTC@)
      --  Multiply by 'pPrice' to change unit to "source currency" (/USD/ in above example).
    , _pPath :: !PathDescr
      -- ^ Actual path
    } deriving (Eq, Show, Generic, Functor)

instance Ord numType => Ord (Path' numType) where
    p1 `compare` p2 =
        let mkTuple p = (_pQty p, _pPrice p, _pPath p)
        in mkTuple p1 `compare` mkTuple p2

instance PrettyVal numType => PrettyVal (Path' numType)
instance NFData numType => NFData (Path' numType)

type Path = Path' NumType

-- | A 'Path' whose /source/ (/start/) currency is a numeraire.
newtype BuyPath' numType = BuyPath' { getBuyPath :: Path' numType }
    deriving (Eq, Show, Ord, Generic, Functor)

type BuyPath = BuyPath' NumType
instance NFData numType => NFData (BuyPath' numType)

-- | A 'Path' whose /destination/ (/end/) currency is a numeraire.
newtype SellPath' numType = SellPath' { getSellPath :: Path' numType }
    deriving (Eq, Show, Ord, Generic, Functor)

toBuyPath
    :: Fractional numType
    => Path' numType
    -> BuyPath' numType
toBuyPath = BuyPath'

toSellPath
    :: Path' numType
    -> SellPath' numType
toSellPath = SellPath'

instance PrettyVal numType => PrettyVal (SellPath' numType)
instance NFData numType => NFData (SellPath' numType)

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
    pPrice :: path -> numType -- ^ Unit: numeraire per crypto
    pQty :: path -> numType -- ^ Unit: numeraire
    toSellOrder :: path -> SomeSellOrder' numType
    showPathQty :: path -> T.Text -- ^ format: "<qty> @ <price> <showPath>"
    pathPath :: path -> Path' numType

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

instance Show numType => HasPathQuantity (Path' numType) numType where
    pathPath = id
    pPrice = _pPrice
    pQty = _pQty
    toSellOrder = pathToSellOrder
    showPathQty = pathShowPathQty

pathToSellOrder :: Path' numType -> SomeSellOrder' numType
pathToSellOrder path = SomeSellOrder'
    { soPrice = _pPrice path
    , soQty   = _pQty path
    , soBase  = snd $ NE.last (_pMoves $ _pPath path)
    , soQuote = _pStart $ _pPath path
    , soVenue = showPath path
    }

pathShowPathQty :: Show a => Path' a -> Text
pathShowPathQty path =
    toS (printf "%s @ %s %s" (show $ _pQty path) (show $ _pPrice path) (showPath path) :: String)

instance HasPath (BuyPath' numType) where
    pathDescr = pathDescr . getBuyPath
    showPath = showPath . getBuyPath

instance (Show numType, Num numType) => HasPathQuantity (BuyPath' numType) numType where
    pathPath = getBuyPath
    pPrice = _pPrice . getBuyPath
    pQty bp = _pQty (getBuyPath bp) * _pPrice (getBuyPath bp)
    toSellOrder = pathToSellOrder . getBuyPath
    showPathQty = pathShowPathQty . getBuyPath

instance HasPath (SellPath' numType) where
    pathDescr = pathDescr . getSellPath
    showPath = showPath . getSellPath

instance HasPathQuantity SellPath NumType where
    pathPath = getSellPath
    pPrice sp = recip $ _pPrice (getSellPath sp)
    pQty = _pQty . getSellPath
    toSellOrder = invertSomeSellOrder . pathToSellOrder . getSellPath
    showPathQty = pathShowPathQty . getSellPath

instance HasPathQuantity (SellPath' Double) Double where
    pathPath = getSellPath
    pPrice sp = recip $ _pPrice (getSellPath sp)
    pQty = _pQty . getSellPath
    toSellOrder = fmap realToFrac . toSellOrder . fmap toRational
    showPathQty = pathShowPathQty . getSellPath
