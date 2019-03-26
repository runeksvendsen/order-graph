{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OrderBook.Graph.Build
( module OrderBook.Graph.Types
, SellOrderGraph
, SortedOrders, first, rest, prepend, toList, replaceHead
, build
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types

import qualified Data.Graph.Digraph                         as DG
import           Data.List                                  (groupBy, sortOn)
import qualified Data.List.NonEmpty                         as NE


type SellOrderGraph s g = DG.Digraph s g SortedOrders Currency

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

-- ^ build a graph where each edge is a list of sorted sell orders
build
    :: (PrimMonad m)
    => SellOrderGraph (PrimState m) g   -- ^ Empty graph
    -> [SomeSellOrder]                  -- ^ Orders
    -> m ()
build mGraph orders = do
    forM_ (create orders) (DG.insertEdge mGraph)

-- |
create
    :: [SomeSellOrder]  -- ^ A bunch of sell orders
    -> [SortedOrders]
create =
    fmap (SortedOrders . NE.fromList . sortOn soPrice) . groupBy sameSrcDst
        . fmap assertPositivePrice
  where
    assertPositivePrice order
        | soPrice order >= 0 = order
        | otherwise = error $ "negative-price order: " ++ show order
    sameSrcDst oA oB =
        soBase oA == soBase oB && soQuote oA == soQuote oB
