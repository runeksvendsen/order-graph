{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OrderBook.Graph
( module Export
, matchOrders
)
where

import OrderBook.Graph.Internal.Prelude

import OrderBook.Graph.Types        as Export
import OrderBook.Graph.Build        as Export
import OrderBook.Graph.Exchange     as Export
import OrderBook.Graph.Match        as Export
import OrderBook.Graph.Query        as Export
import OrderBook.Graph.Run          as Export

import qualified Data.Graph.Digraph                         as DG


matchOrders
    :: forall numType src dst s.
       (KnownSymbol src, KnownSymbol dst, Real numType)
    => (forall m. Monad m => String -> m ())
    -> BuyOrder dst src     -- ^ Buy cryptocurrency for national currency
    -> BuyOrder src dst     -- ^ Sell cryptocurrency for national currency
    -> [OrderBook numType]      -- ^ Input orders
    -> ST s ([SomeSellOrder], [SomeSellOrder])    -- ^ (bids, asks)
matchOrders log' buyOrder sellOrder orderBooks =
    DG.withGraph $ \mGraph -> do
        log' "Building graph..."
        build mGraph orderBooks
        graphInfo :: GraphInfo numType <- graphInfo mGraph
        let vertexCount = length (giVertices graphInfo)
            edgeCount = giEdgeCount graphInfo
        log' $ "Vertex count: " ++ show vertexCount
        log' $ "Edge count:   " ++ show edgeCount
        -- Arbitrages
        buyGraph <- runArb mGraph $ do
            log' "Finding arbitrages..."
            -- Asks
            (buyGraph, arbs) <- arbitrages sellOrder
            -- Finds all arbitrages (regardless of "src" vertex)
            log' $ unlines ["Arbitrages:", pp arbs]
            return buyGraph
        -- Match
        runMatch buyGraph $ do
            log' "Matching sell order..."
            bids <- match sellOrder -- map invertSomeSellOrder <$> match sellOrder
            log' "Matching buy order..."
            asks <- match buyOrder
            return undefined --(bids, asks)
