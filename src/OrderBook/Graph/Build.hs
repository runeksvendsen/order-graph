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
, CompactOrderList, toSortedOrders, fromSortedOrders
, Tagged(..)
, build
, buildFromOrders
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types
import           OrderBook.Graph.Types.SortedOrders
import qualified OrderBook.Graph.Types.Book                 as Book

import qualified Data.Graph.Digraph                         as DG
import           Data.List                                  (groupBy, sortOn)


type SellOrderGraph s kind = DG.Digraph s Currency (Tagged kind CompactOrderList)

-- ^ build a graph where each edge is a list of sorted sell orders
build
    :: (Real numType)
    => [Book.OrderBook numType]            -- ^ Order books
    -> ST s (SellOrderGraph s "arb")
build orders =
    let edges = map Tagged (create orders)
    in DG.fromEdges edges

-- |
create
    :: Real numType
    => [Book.OrderBook numType]  -- ^ A bunch of order books
    -> [SortedOrders]
create =
    createFromOrders . concat . concat . map ((\(a1, a2) -> a1 : [a2]) . Book.toSellBuyOrders)

-- ^ Same as 'build' but take orders (instead of order books) as input.
-- Only present for backwards compatibility (slower than 'build').
buildFromOrders
    :: [SomeSellOrder]                      -- ^ Orders
    -> ST s (SellOrderGraph s "arb")
buildFromOrders orders =
    let edges = map Tagged (createFromOrders orders)
    in DG.fromEdges edges

-- |
createFromOrders
    :: [SomeSellOrder]  -- ^ A bunch of sell orders
    -> [SortedOrders]
createFromOrders =
    mapMaybe mkSortedOrders . groupByMarket . fmap assertPositivePrice
  where
    assertPositivePrice order
        | soPrice order >= 0 = order
        | otherwise = error $ "negative-price order: " ++ show order
    groupByMarket = groupBy (\o1 o2 -> oBaseQuote o1 == oBaseQuote o2) . sortOn oBaseQuote
    oBaseQuote o = (soBase o, soQuote o)
