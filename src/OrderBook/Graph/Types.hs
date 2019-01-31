{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module OrderBook.Graph.Types
( Edge(..)
, IsEdge(..)
, Currency
, SomeSellOrder'(..), someOrder
, SomeSellOrder, SomeBuyOrder
, OrderGraph
, GraphM
, matchedOrder
)
where

import           OrderBook.Graph.Internal.Prelude
import qualified Data.Text                                  as T
import qualified Data.Graph.Types                           as G
import qualified Control.Monad.Trans.State.Strict           as S
import           Data.Semigroup                             (Product(..), Min(..))
import qualified Data.List.NonEmpty                         as NE
import           Data.String                                (IsString)
import           Data.Hashable                              (Hashable)


-- | An edge in a graph
class ( Eq nodeLabel
      , Ord edge
      , Hashable nodeLabel
      ) => IsEdge edge nodeLabel | edge -> nodeLabel where
    fromNode :: edge -> nodeLabel   -- ^ Label associated with the edge's "from" node
    toNode   :: edge -> nodeLabel   -- ^ Label associated with the edge's "to" node
    weight   :: edge -> Double      -- ^ Edge's weight

-- | An edge in a graph.
-- Used to create alternative implementations of
--  e.g. 'Eq' and 'Ord' for types used as graph edges.
-- The "weight factor" is used to normalize edge weights
--  to get around Dijkstra not supporting negative edge weights
--  (or edge weights below 1.0, since we combine weights by multiplication)
data Edge a = Edge { getEdge :: a, getWeightFactor :: Double }
    deriving (Read, Show, Generic, Functor)

-- | Currency code, e.g. "EUR", "BTC", "USD", "ETH"
newtype Currency = Currency T.Text
    deriving (Eq, Ord, Read, Show, IsString, Semigroup, Monoid, Hashable, Generic)

instance NFData Currency

-- | A sell order.
--   An offer to exchange 'qty' of 'soBase' for 'soQuote',
--    at 'price' (which has unit: 'soQuote' per 'soBase').
data SomeSellOrder' price qty =
    SomeSellOrder'
    { soPrice :: price
    , soQty   :: qty
    , soBase  :: Currency
    , soQuote :: Currency
    , soVenue :: T.Text
    } deriving (Read, Show, Generic)

instance (NFData price, NFData qty) => NFData (SomeSellOrder' price qty)

instance Eq (Edge SomeSellOrder) where
    e1 == e2 =
        weight e1 == weight e2

instance Ord (Edge SomeSellOrder) where
    e1 <= e2 =
        weight e1 <= weight e2

instance IsEdge (Edge SomeSellOrder) Currency where
    -- We move in the opposite direction of the seller
    fromNode (Edge order _) = soQuote order
    toNode (Edge order _) = soBase order
    weight (Edge order normFac) = soPrice order * normFac



type SomeSellOrder = SomeSellOrder' Double Double
type SomeOrderSemigroup = SomeSellOrder' (Product Double) (Min Double)


-- | An offer to buy 'base' in exchange for 'quote'
type SomeBuyOrder = SomeSellOrder

someOrder :: SomeOrderSemigroup -> SomeSellOrder
someOrder so@SomeSellOrder'{..} =
    so { soPrice = getProduct soPrice, soQty = getMin soQty}

someOrderSemigroup :: SomeSellOrder -> SomeOrderSemigroup
someOrderSemigroup so@SomeSellOrder'{..} =
    so { soPrice = Product soPrice, soQty = Min soQty}

matchedOrder :: NonEmpty SomeSellOrder -> SomeSellOrder
matchedOrder = someOrder . foldr1 (<>) . NE.map someOrderSemigroup

-- | A graph with currencies as nodes and sell orders as edges
type OrderGraph gr = G.Graph gr (Edge SomeSellOrder) Currency

-- | A graph monad with currencies as nodes and sell orders as edges
type GraphM gr = S.State (OrderGraph gr)

-- | Comparing price only
instance Eq price => Eq (SomeSellOrder' price qty) where
    so1 == so2 =
        soPrice so1 == soPrice so2

-- | Comparing price only
instance Ord price => Ord (SomeSellOrder' price qty) where
    so1 <= so2 =
        soPrice so1 <= soPrice so2

instance (Num price, Fractional qty, Ord qty, Show price, Show qty, qty ~ price)
    => Monoid (SomeSellOrder' (Product price) (Min qty)) where
        mempty =
            SomeSellOrder'
                (Product 1)
                (Min $ 1/0)
                "Monoid.mempty"
                "Monoid.mempty"
                "Monoid.mempty"

-- | NB: "so1 <> so2" will fail if "soQuote so1 /= soBase so2"
instance (Num price, Ord qty, Fractional price, Show price, Show qty, qty ~ price)
    => Semigroup (SomeSellOrder' (Product price) (Min qty)) where
    -- Example:
    --                     USD per BTC                                    ETH per USD
    --           BTCUSD      /     BTC   USD                    USDETH      /     USD   ETH
        so1@(SomeSellOrder' p1 q1 base1 quote1 v1) <> so2@(SomeSellOrder' p2 q2 base2 quote2 v2)
    --                          \                                                \
    --                          BTC                                              USD
            | quote1 == base2 =
    --          BTCETH
                SomeSellOrder'
                    (p1 <> p2)      -- ETH per BTC
                    (q1 <> so2qty)  -- BTC
                    base1           -- BTC
                    quote2          -- ETH
                    (v1 <> "," <> v2)
            | otherwise = error $ "Incompatible orders: " ++ show (so1, so2)
                where so2qty = fmap (/ getProduct p1) q2
