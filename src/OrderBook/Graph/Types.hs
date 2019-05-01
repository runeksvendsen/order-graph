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
, DG.DirectedEdge(..)
, GE.WeightedEdge(..)
, Currency
, SomeSellOrder'(..)
, SomeSellOrder
, NumType
)
where

import           OrderBook.Graph.Internal.Prelude

import qualified Data.Graph.Digraph                         as DG
import qualified Data.Graph.Edge                            as GE
import qualified Data.Text                                  as T
import           Data.String                                (IsString)


-- | An edge in a graph.
-- Used to create alternative implementations of
--  e.g. 'Eq' and 'Ord' for types used as graph edges.
data Edge a = Edge
    { getEdge :: a }
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
instance StringConv Currency T.Text where
    strConv _ (Currency txt) = txt
instance StringConv T.Text Currency where
    strConv _ = Currency

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
    } deriving (Eq, Read, Ord, Functor, Generic)

type NumType = Rational
type SomeSellOrder = SomeSellOrder' NumType

instance Real numType => Show (SomeSellOrder' numType) where
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
    Edge e1 == Edge e2 =
        GE.weight e1 == GE.weight e2

instance Ord (Edge SomeSellOrder) where
    Edge e1 <= Edge e2 =
        GE.weight e1 <= GE.weight e2

instance DG.DirectedEdge (SomeSellOrder' numType) Currency where
    -- We move in the opposite direction of the seller
    fromNode = soQuote
    toNode = soBase

instance GE.WeightedEdge (SomeSellOrder' numType) Currency numType where
    weight = soPrice
