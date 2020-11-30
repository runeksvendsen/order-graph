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
, toDouble
, toCompactOrder
, NumType
, fromCompactOrder
, sedgewickWayneFormat
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
    } deriving (Eq, Show, Read, Ord, Functor, Generic)

type NumType = Rational
type SomeSellOrder = SomeSellOrder' NumType

-- instance Real numType => Show (SomeSellOrder' numType) where
--     show SomeSellOrder'{..} =
--         printf "Order { %s qty=%f price=%f %s/%s }"
--             soVenue
--             (realToFrac soQty :: Double)
--             (realToFrac soPrice :: Double)
--             (toS soBase :: String)
--             (toS soQuote :: String)

instance NFData numType => NFData (SomeSellOrder' numType)
instance PrettyVal (SomeSellOrder' Double)

instance PrettyVal (SomeSellOrder' Rational) where
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
    } deriving (Eq, Read, Ord, Functor, Generic)

instance Real numType => Show (CompactOrder' numType) where
    show CompactOrder'{..} =
        printf "CompactOrder { %s qty=%f price=%f }"
            coVenue
            (realToFrac coQty :: Double)
            (realToFrac coPrice :: Double)

instance PrettyVal (CompactOrder' Double)
type CompactOrder = CompactOrder' NumType

toDouble :: Real numType => CompactOrder' numType -> CompactOrder' Double
toDouble = fmap realToFrac

instance NFData numType => NFData (CompactOrder' numType)

instance DG.HasWeight CompactOrder Double where
    weight = fromRational . coPrice

toCompactOrder :: SomeSellOrder' numType -> CompactOrder' numType
toCompactOrder o = CompactOrder' (soPrice o) (soQty o) (soVenue o)

fromCompactOrder :: DG.IdxEdge Currency (CompactOrder' numType) -> SomeSellOrder' numType
fromCompactOrder idxEdge =
    let base = DG.toNode idxEdge
        quote = DG.fromNode idxEdge
        co = DG.eMeta idxEdge
    in SomeSellOrder'
        { soPrice = coPrice co
        , soQty   = coQty co
        , soBase  = base
        , soQuote = quote
        , soVenue = coVenue co
        }

sedgewickWayneFormat :: Int -> [DG.IdxEdge Currency CompactOrder] -> [String]
sedgewickWayneFormat vertexCount lst =
    (show vertexCount) :
    (show $ length lst) :
    (map sedgewickWayneFormatSingle lst)

sedgewickWayneFormatSingle :: DG.IdxEdge Currency CompactOrder -> String
sedgewickWayneFormatSingle idxEdge =
    printf "%d %d  %f"
        (DG.vidInt $ DG.eFromIdx idxEdge)
        (DG.vidInt $ DG.eToIdx idxEdge)
        (DG.weight $ DG.eMeta idxEdge :: Double)
