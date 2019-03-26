{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OrderBook.Graph.Types.SortedOrders
( SortedOrders(..), first, rest, prepend, toList, replaceHead
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types
import qualified Data.List.NonEmpty                         as NE


-- | A list of sell orders sorted (ascending) by price
newtype SortedOrders = SortedOrders { getOrders :: NE.NonEmpty SomeSellOrder }
    deriving (Eq, Show, Generic)

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

instance DirectedEdge SortedOrders Currency where
    fromNode = fromNode . NE.head . getOrders
    toNode = toNode . NE.head . getOrders

instance WeightedEdge SortedOrders Currency Double where
    weight = fromRational . weight . NE.head . getOrders
