{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Types.Path
( BuyPath, pPrice, pQty
, toPath
, showPath
, BuyPathD
, BuyPathR
)
where

import OrderBook.Graph.Internal.Prelude
import OrderBook.Graph.Types (Currency, SomeSellOrder'(..))
import OrderBook.Graph.Exchange (rawQty, rawPrice, Order', oQty, oPrice)
import qualified OrderBook.Graph.Query                      as Query
import qualified OrderBook.Graph.Build                      as B
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE


-- | A path through multiple markets as seen from the /buyers/ perspective
data BuyPath numType = BuyPath
    { _pPrice :: numType
    , _pQty   :: numType
    , _pStart :: Currency -- ^ Start vertex
    , _pMoves :: NonEmpty (T.Text, Currency) -- ^ (venue, vertex)-list
    } deriving (Eq, Show)

type BuyPathD = BuyPath Double
type BuyPathR = BuyPath Rational

pPrice :: BuyPath numType -> numType
pPrice = _pPrice

pQty :: BuyPath numType -> numType
pQty = _pQty

-- |
toPath
    :: Order' numType base quote
    -- ^ The sell orders in the path compressed into a single order whose quantity
    --   is the maximum capacity of the path in question
    -> Query.ShortestPath
    -- ^ Sell orders in the path
    -> BuyPath numType
toPath maxOrder (Query.ShortestPath sortedOrders) = BuyPath
    (rawPrice $ oPrice maxOrder)
    (rawQty $ oQty maxOrder)
    (soQuote first)
    (NE.map venueAndBase revSellOrders)
  where
    sellOrders = fmap B.first sortedOrders
    revSellOrders@(first NE.:| _) = NE.reverse sellOrders
    venueAndBase so = (soVenue so, soBase so)

showPath :: BuyPath numType -> T.Text
showPath path =
    toS (_pStart path) <> T.concat (NE.toList moves)
  where
    moves = NE.map venueAndBase (_pMoves path)
    venueAndBase :: (T.Text, Currency) -> T.Text
    venueAndBase (venue, base) = " --" <> venue <> "--> " <> toS base
