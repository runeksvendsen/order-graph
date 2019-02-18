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
, buildEdges
, IsEdge(..)
, Currency
, SomeSellOrder'(..)
, SomeSellOrder
)
where

import           OrderBook.Graph.Internal.Prelude
import qualified Data.Text                                  as T
import           Data.String                                (IsString)
import           Data.Hashable                              (Hashable)


-- | An edge in a graph
class ( Eq nodeLabel
      , Ord edge
      , Hashable nodeLabel
      ) => IsEdge edge nodeLabel | edge -> nodeLabel where
    fromNode :: edge -> nodeLabel   -- ^ Label associated with the edge's "from" node
    toNode   :: edge -> nodeLabel   -- ^ Label associated with the edge's "to" node
    weight   :: edge -> Rational    -- ^ Edge's weight

-- | An edge in a graph.
-- Used to create alternative implementations of
--  e.g. 'Eq' and 'Ord' for types used as graph edges.
data Edge a = Edge
    { getEdge :: a
    , getNormalizationFactor :: Rational
    -- ^ The "weight factor" is used because Dijkstra does not support negative edge weights.
    -- However, since we're doing multiplication of edge weights (rather than addition)
    -- the edge weight must be greater than or eqaul to 1.
    }
    deriving (Read, Generic, Functor)

instance Show a => Show (Edge a) where
    show = show . getEdge

instance PrettyVal a => PrettyVal (Edge a) where
    prettyVal = prettyVal . getEdge

-- | Currency code, e.g. "EUR", "BTC", "USD", "ETH"
newtype Currency = Currency T.Text
    deriving (Eq, Ord, Read, IsString, Semigroup, Monoid, Hashable, Generic, PrettyVal)

instance Show Currency where
    show (Currency txt) = toS txt
instance NFData Currency
instance StringConv String Currency where
    strConv _ = fromString
instance StringConv Currency String where
    strConv l (Currency txt) = strConv l txt

-- | A sell order.
--   An offer to exchange 'soQty' of 'soBase' for 'soQuote',
--    at 'soPrice' (which has unit: 'soQuote' per 'soBase').
data SomeSellOrder' numType =
    SomeSellOrder'
    { soPrice :: numType
    , soQty   :: numType
    , soBase  :: Currency
    , soQuote :: Currency
    , soVenue :: T.Text
    } deriving (Eq, Read, Functor, Generic)

instance Show SomeSellOrder where
    show SomeSellOrder'{..} =
        printf "Order { %s qty=%f price=%f %s/%s }"
            soVenue
            (realToFrac soQty :: Double)
            (realToFrac soPrice :: Double)
            (toS soBase :: String)
            (toS soQuote :: String)

instance NFData numType => NFData (SomeSellOrder' numType)
instance PrettyVal (SomeSellOrder' Double)

instance PrettyVal SomeSellOrder where
    prettyVal sso =
        prettyVal (fmap realToFrac sso :: SomeSellOrder' Double)

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

buildEdges
    :: [SomeSellOrder]
    -> [Edge SomeSellOrder]
buildEdges orders =
    let minPrice = foldl1 min (map soPrice orders)
        -- NB: exception if minOrderPrice==0
        normFactor = max 1.0 (roundUp $ 1.0 / minPrice)
        roundUp = fromInteger . ceiling
    in map (`Edge` normFactor) orders


type SomeSellOrder = SomeSellOrder' Rational
