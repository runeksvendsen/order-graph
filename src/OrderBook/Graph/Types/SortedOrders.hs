{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OrderBook.Graph.Types.SortedOrders
( SortedOrders, mkSortedOrders, first, rest, prepend, toList, replaceHead
, CompactOrderList'(..), toSortedOrders, fromSortedOrders
, CompactOrderList, compactOrderListDouble, compactOrderListHead
, Tag.Tagged(..)
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types
import qualified Data.Graph.Digraph                         as DG

import qualified Data.List.NonEmpty                         as NE
import qualified Data.Tagged                                as Tag
import Data.List (sortOn)


type CompactOrderList = CompactOrderList' NumType

newtype CompactOrderList' numType = CompactOrderList' { getCompactOrders' :: NE.NonEmpty (CompactOrder' numType) }
    deriving (Eq, Show, Functor, Generic, NFData)

compactOrderListDouble :: Real numType => CompactOrderList' numType -> CompactOrderList' Double
compactOrderListDouble = fmap realToFrac

compactOrderListHead :: Real numType => CompactOrderList' numType -> CompactOrder' numType
compactOrderListHead = NE.head . getCompactOrders'

instance PrettyVal (CompactOrderList' Double)

instance DG.HasWeight CompactOrderList Double where
    weight = DG.weight . NE.head . getCompactOrders'

instance DG.HasWeight (Tag.Tagged kind CompactOrderList) Double where
    weight = DG.weight . Tag.unTagged

-- TODO: use 'toSellOrder'
toSortedOrders :: DG.IdxEdge Currency CompactOrderList -> DG.IdxEdge Currency SortedOrders
toSortedOrders idxEdge =
    let quote = fromNode idxEdge
        base = toNode idxEdge
        compactOrders = fmap getCompactOrders' idxEdge
        mkOrder' co = SomeSellOrder'
            { soPrice = coPrice co
            , soQty   = coQty co
            , soBase  = base
            , soQuote = quote
            , soVenue = coVenue co
            }
    in fmap (SortedOrders . NE.map mkOrder') compactOrders

fromSortedOrders :: DG.IdxEdge Currency SortedOrders -> DG.IdxEdge Currency CompactOrderList
fromSortedOrders idxSo =
    fmap (CompactOrderList' . NE.map toCompactOrder . getOrders) idxSo

-- | A list of sell orders sorted (ascending) by price
newtype SortedOrders = SortedOrders { getOrders :: NE.NonEmpty SomeSellOrder }
    deriving (Eq, Show, Generic)

mkSortedOrders :: [SomeSellOrder] -> Maybe SortedOrders
mkSortedOrders = fmap SortedOrders . NE.nonEmpty . sortOn soPrice

instance PrettyVal SortedOrders

first
    :: SortedOrders
    -> SomeSellOrder
first = NE.head . getOrders

rest
    :: SortedOrders
    -> Maybe SortedOrders
rest = fmap SortedOrders . snd . NE.uncons . getOrders

prepend
    :: SomeSellOrder
    -> SortedOrders
    -> SortedOrders
prepend so (SortedOrders orders) = SortedOrders (so `NE.cons` orders)

toList
    :: SortedOrders
    -> [SomeSellOrder]
toList = NE.toList . getOrders

replaceHeadNE
    :: NE.NonEmpty a
    -> Maybe a
    -> Maybe (NE.NonEmpty a)
replaceHeadNE ne Nothing =
    snd $ NE.uncons ne
replaceHeadNE (_ NE.:| tail') (Just a) =
    Just (a NE.:| tail')

replaceHead
    :: SortedOrders
    -> Maybe SomeSellOrder
    -> Maybe SortedOrders
replaceHead (SortedOrders ne) =
    fmap SortedOrders . replaceHeadNE ne

instance DirectedEdge (Tag.Tagged a SortedOrders) Currency (Tag.Tagged a CompactOrderList) where
    fromNode = fromNode . NE.head . getOrders . Tag.unTagged
    toNode = toNode . NE.head . getOrders . Tag.unTagged
    metaData = Tag.Tagged . CompactOrderList' . NE.map toCompactOrder . getOrders . Tag.unTagged
