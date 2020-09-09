{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Types.Path
( BuyPath, bpPrice, bpQty
, toPath
, toSellOrder
, showPath
, BuyPathD
, BuyPathR
)
where

import OrderBook.Graph.Internal.Prelude
import OrderBook.Graph.Types.Currency (Currency)
import OrderBook.Graph.Types.SomeSellOrder (SomeSellOrder'(..))
import OrderBook.Graph.Exchange (rawQty, rawPrice, Order', oQty, oPrice)
import qualified OrderBook.Graph.Build                      as B
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE


-- | A path starting at a cryptocurrency and ending at a national currency (numeraire)
data BuyPath numType = BuyPath
    { _pPrice :: numType
    , _pQty   :: numType
    , _pStart :: Currency -- ^ Start vertex
    , _pMoves :: NonEmpty (T.Text, Currency) -- ^ (venue, vertex)-list
    } deriving (Eq, Show, Generic)

instance PrettyVal numType => PrettyVal (BuyPath numType)

type BuyPathD = BuyPath Double
type BuyPathR = BuyPath Rational

bpPrice :: BuyPath numType -> numType
bpPrice = _pPrice

bpQty :: BuyPath numType -> numType
bpQty = _pQty

newtype SellPath numType = SellPath { getBuyPath :: BuyPath numType }
  deriving (Eq, Show, Generic)

instance PrettyVal numType => PrettyVal (SellPath numType)

type SellPathD = SellPath Double
type SellPathR = SellPath Rational

spPrice :: SellPath numType -> numType
spPrice = _pPrice . getBuyPath

spQty :: SellPath numType -> numType
spQty = _pQty . getBuyPath

-- |
toPath
    :: Order' numType base quote
    -- ^ The result of
    --   @OrderBook.Graph.Exchange.withSomeSellOrders sp $ \_ orders -> Exchange.maxOrder orders@
    -> NonEmpty B.SortedOrders
    -- ^ The first argument to @f@ in @OrderBook.Graph.Exchange.withSomeSellOrders sp f@
    -> BuyPath numType
toPath maxOrder revSortedOrders = BuyPath
    (rawPrice $ oPrice maxOrder)
    (rawQty $ oQty maxOrder)
    (soQuote first)
    (NE.map venueAndBase revSellOrders)
  where
    revSellOrders@(first NE.:| _) = fmap B.first (NE.reverse revSortedOrders)
    venueAndBase so = (soVenue so, soBase so)

showPath :: BuyPath numType -> T.Text
showPath path =
    toS (_pStart path) <> T.concat (NE.toList moves)
  where
    moves = NE.map venueAndBase (_pMoves path)
    venueAndBase :: (T.Text, Currency) -> T.Text
    venueAndBase (venue, base) = " --" <> venue <> "--> " <> toS base

toSellOrder :: BuyPath numType -> SomeSellOrder' numType
toSellOrder bp = SomeSellOrder'
    { soPrice = bpPrice bp
    , soQty   = bpQty bp
    , soBase  = snd $ NE.last (_pMoves bp)
    , soQuote = _pStart bp
    , soVenue = showPath bp
    }

pathInvert
  :: BuyPath numType
  -> SellPath numType
pathInvert bp = undefined
