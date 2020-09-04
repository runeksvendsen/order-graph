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
module OrderBook.Graph.Types.SomeSellOrder
( DG.DirectedEdge(..)
, Currency
, SomeSellOrder'(..)
, SomeSellOrder
, CompactOrder'(..)
, CompactOrder
, toCompactOrder
, NumType
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types.Currency

import qualified Data.Graph.Digraph                         as DG
import qualified Data.Text                                  as T


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

instance DG.DirectedEdge (SomeSellOrder' numType) Currency (CompactOrder' numType) where
    -- We move in the opposite direction of the seller
    fromNode = soQuote
    toNode = soBase
    metaData o = CompactOrder' (soPrice o) (soQty o) (soVenue o)

-- | An order without "base" and "quote".
-- Useful in a context where "base" and "quote" are implied,
--  e.g. as edges in a graph where a vertex signifies base/quote.
data CompactOrder' numType = CompactOrder'
    { coPrice :: numType
    , coQty   :: numType
    , coVenue :: T.Text
    } deriving (Eq, Show, Read, Ord, Functor, Generic)

type CompactOrder = CompactOrder' NumType

instance NFData numType => NFData (CompactOrder' numType)

instance DG.HasWeight CompactOrder Double where
    weight = log . fromRational . coPrice

toCompactOrder :: SomeSellOrder' numType -> CompactOrder' numType
toCompactOrder o = CompactOrder' (soPrice o) (soQty o) (soVenue o)
