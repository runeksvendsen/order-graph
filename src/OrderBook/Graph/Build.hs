{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OrderBook.Graph.Build
( module OrderBook.Graph.Types
, build
, derive
)

where

import OrderBook.Graph.Internal.Prelude
import OrderBook.Graph.Types
import Control.Monad
import Control.Monad.Primitive                              (PrimMonad, PrimState)
import qualified Data.Primitive.MutVar                      as Prim
import qualified Data.Graph.Types                           as G
import qualified Data.Graph.Immutable                       as GI
import qualified Data.Graph.Mutable                         as GM
import qualified Data.Heap                                  as H


type SellOrderHeap = H.MinHeap (Edge SomeSellOrder)

-- ^ Derive a graph which only contains the best-priced sell
--  order as each edge
derive
    :: G.Graph g SellOrderHeap Currency
    -> G.Graph g (Edge SomeSellOrder) Currency
derive graph =
    GI.mapEdges heapKeepMin graph
  where
    heapKeepMin :: G.Vertex g -> G.Vertex g -> SellOrderHeap -> Edge SomeSellOrder
    heapKeepMin _ _ edgeHeap = fromMaybe
        (error $ "deriveGraph: missing edge")
        (H.viewHead edgeHeap)

build
    :: (PrimMonad m)
    => G.MGraph (PrimState m) g SellOrderHeap Currency      -- ^ Empty graph
    -> [SomeSellOrder]                                      -- ^ Orders
    -> m ()
build mGraph orders = do
    minOrderPriceVar <- Prim.newMutVar (-1 :: Double)
    -- Find minimum sell order price
    forM_ orders $ \order -> do
        Prim.modifyMutVar minOrderPriceVar (min (soPrice order))
    minOrderPrice <- Prim.readMutVar minOrderPriceVar
    -- Calculate normalization factor
    let normFactor = max 1.0 (roundUp $ 1.0 / minOrderPrice)    -- NB: exception if minOrderPrice==0
        roundUp = fromInteger . ceiling
    -- Add edges to graph
    forM_ orders (addEdge mGraph . (`Edge` normFactor))
  where
    addEdge :: (PrimMonad m, IsEdge (Edge SomeSellOrder) v)
            => G.MGraph (PrimState m) g SellOrderHeap v
            -> Edge SomeSellOrder
            -> m ()
    addEdge graph edge = do
        fromVertex <- GM.insertVertex graph (fromNode edge)
        toVertex <- GM.insertVertex graph (toNode edge)
        heapM <- GM.lookupEdge graph fromVertex toVertex
        -- Add edge to heap
        let heap = case heapM of
                Nothing         -> H.singleton edge
                Just orderHeap  -> H.insert edge orderHeap
        -- Replace old edge-heap with new
        GM.insertEdge graph fromVertex toVertex heap
