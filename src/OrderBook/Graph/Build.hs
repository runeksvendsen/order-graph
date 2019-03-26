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
import           OrderBook.Graph.Types.SortedOrders

import qualified Data.Graph.Digraph                         as DG
import           Data.List                                  (groupBy, sortOn, sortBy)
import qualified Data.List.NonEmpty                         as NE


type SellOrderGraph s g = DG.Digraph s g SortedOrders Currency

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
    -- TODO: sort order books instead of orders
    fmap (SortedOrders . NE.fromList . sortOn soPrice) . groupByMarket
        . fmap assertPositivePrice
  where
    assertPositivePrice order
        | soPrice order >= 0 = order
        | otherwise = error $ "negative-price order: " ++ show order
    groupByMarket = groupBy sameSrcDst . sortBy orderSrcDst
    sameSrcDst oA oB =
        soBase oA == soBase oB &&
        soQuote oA == soQuote oB
    orderSrcDst oA oB =
        soBase oA `compare` soBase oB <>
        soQuote oA `compare` soQuote oB
