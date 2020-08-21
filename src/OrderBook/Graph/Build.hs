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
, Tagged(..)
, build
, buildFromOrders
, GraphInfo(..)
, graphInfo
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types
import           OrderBook.Graph.Types.SortedOrders
import qualified OrderBook.Graph.Types.Book                 as Book

import qualified Data.Graph.Digraph                         as DG
import           Data.List                                  (groupBy, sortOn, sortBy)
import qualified Data.List.NonEmpty                         as NE


type SellOrderGraph s g kind = DG.Digraph s g (Tagged kind SortedOrders) Currency

-- ^ build a graph where each edge is a list of sorted sell orders
build
    :: (PrimMonad m, Real numType)
    => SellOrderGraph (PrimState m) g "arb" -- ^ Empty graph
    -> [Book.OrderBook numType]            -- ^ Order books
    -> m ()
build mGraph orders = do
    forM_ (create orders) (DG.insertEdge mGraph . Tagged)

-- |
create
    :: Real numType
    => [Book.OrderBook numType]  -- ^ A bunch of order books
    -> [SortedOrders]
create =
        catMaybes . map (fmap SortedOrders . NE.nonEmpty . sortOn soPrice . doAssertions)
            . concat . map (concatListPairs . toOrders) . groupByMarketVenue
  where
    concatListPairs :: ([[a]], [[a]]) -> [[a]]
    concatListPairs (listA, listB) = listA ++ listB
    toOrders :: Real numType
             => [Book.OrderBook numType]
             -> ([[SomeSellOrder]], [[SomeSellOrder]])
    toOrders aBookLst = foldl
        (\(accumSell, accumBuy) (sellOrders, buyOrders) ->
            (sellOrders : accumSell, buyOrders : accumBuy)
        )
        ([],[])
        (map Book.toSellBuyOrders aBookLst)
    doAssertions =
        assertSameBaseQuoteVenue . map assertPositivePrice
    assertSameBaseQuoteVenue lst =
        if all (sameBaseQuoteVenueOrder (head lst)) lst
            then lst
            else error $ "SortedOrders with different base/quote: " ++ show lst
    sameBaseQuoteVenueOrder o1 o2 =
        soBase o1 == soBase o2
        && soQuote o1 == soQuote o2
        && soVenue o1 == soVenue o2
    assertPositivePrice order
        | soPrice order >= 0 = order
        | otherwise = error $ "negative-price order: " ++ show order
    baseQuoteVenue ob = (Book.baseQuote ob, Book.bookVenue ob)
    groupByMarketVenue :: [Book.OrderBook numType] -> [[Book.OrderBook numType]]
    groupByMarketVenue = groupBy sameBaseQuoteVenue . sortOn baseQuoteVenue
    sameBaseQuoteVenue :: Book.OrderBook numType -> Book.OrderBook numType -> Bool
    sameBaseQuoteVenue ob1 ob2 = baseQuoteVenue ob1 == baseQuoteVenue ob2

-- ^ Same as 'build' but take orders (instead of order books) as input.
-- Only present for backwards compatibility (slower than 'build').
buildFromOrders
    :: (PrimMonad m)
    => SellOrderGraph (PrimState m) g "arb" -- ^ Empty graph
    -> [SomeSellOrder]                      -- ^ Orders
    -> m ()
buildFromOrders mGraph orders = do
    forM_ (createFromOrders orders) (DG.insertEdge mGraph . Tagged)

-- |
createFromOrders
    :: [SomeSellOrder]  -- ^ A bunch of sell orders
    -> [SortedOrders]
createFromOrders =
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

graphInfo
    :: (PrimMonad m, Real numType)
    => SellOrderGraph (PrimState m) g "arb"
    -> m (GraphInfo numType)
graphInfo graph = do
    currencies <- DG.vertexLabels graph
    edgeCount <- DG.edgeCount graph
    return $ GraphInfo
        { giVertices    = currencies
        , giEdgeCount   = edgeCount
        }

-- NB: Phantom 'numType' is number type of input order book
data GraphInfo numType = GraphInfo
    { giVertices    :: [Currency]
    , giEdgeCount   :: Word
    }
