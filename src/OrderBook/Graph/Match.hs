{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Match
( match
, BuyOrder
, BuyOrder'(..)
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types                      ( IsEdge(..), Edge(..), Currency
                                                            )
import           OrderBook.Graph.Build                      ( SomeSellOrder
                                                            , SomeSellOrder'(..)
                                                            )
import qualified OrderBook.Graph.Build                      as Build
import qualified OrderBook.Graph.Query                      as Query
import qualified OrderBook.Graph.Exchange                   as Exchange

import qualified Data.Graph.Types                           as G
import qualified Data.Graph.Mutable                         as GM
import qualified Data.Graph.Immutable                       as GI
import qualified Data.List.NonEmpty                         as NE
import qualified Data.Heap                                  as H
import qualified Data.Text                                  as T


-- | 
data BuyOrder' numTyp (dst :: Symbol) (src :: Symbol) = BuyOrder'
    { boQuantity        :: numTyp
    , boMaxPrice        :: Maybe numTyp
      -- ^ (TODO: IGNORED FOR NOW) maximum price
    , boMaxSlippage     :: Maybe numTyp
      -- ^ (per-market) maximum percentage difference
      -- between price of first and last matched order
    }

type BuyOrder = BuyOrder' Rational

match
    :: forall m g base quote.
       (PrimMonad m, KnownSymbol base, KnownSymbol quote)
    => G.MGraph (PrimState m) g Build.SellOrderHeap Currency
    -> BuyOrder base quote
    -> m [SomeSellOrder]
match g = fmap reverse . matchR [] g

matchR
    :: forall m g base quote.
       (PrimMonad m, KnownSymbol base, KnownSymbol quote)
    => [SomeSellOrder]
    -> G.MGraph (PrimState m) g Build.SellOrderHeap Currency
    -> BuyOrder base quote
    -> m [SomeSellOrder]
matchR matchedOrdersR mGraph bo@BuyOrder'{..} = do
    graph <- Build.derive mGraph
    let getVertex v = justOrFail ("Vertex not found", v) (GI.lookupVertex v graph)
    case Query.query graph src dst of
        Nothing        -> return matchedOrdersR
        Just orderPath -> do
            let (newEdges, matchedOrder) = subtractMatchedQty orderPath
            forM_ newEdges (updateEdgeHeap getVertex)
            -- TODO: check BuyOrder quantity and maxPrice
            matchR (matchedOrder : matchedOrdersR) mGraph bo
  where
    updateEdgeHeap
        :: (Currency -> G.Vertex g)     -- ^ Get a vertex from a vertex label
        -> Edge SomeSellOrder           -- ^ Updated top order
        -> m ()
    updateEdgeHeap getVertex matchedEdge = do
        let fromLabel = fromNode matchedEdge
            toLabel = toNode matchedEdge
            from = getVertex fromLabel
            to = getVertex toLabel
        -- The edge should always be present in the graph if it's returned by 'Query.query'
        orderHeapM <- GM.lookupEdge mGraph from to
        let orderHeap = justOrFail ("Edge not in graph", (fromLabel,toLabel)) orderHeapM
        let updatedHeap = replaceSubtractedOrder orderHeap matchedEdge
        case H.isEmpty updatedHeap of
            True ->  GM.removeEdge mGraph from to
            False -> GM.insertEdge mGraph from to updatedHeap
    replaceSubtractedOrder
        :: H.MinHeap (Edge SomeSellOrder)   -- Heap, with old order on top
        -> Edge SomeSellOrder               -- New top order (if qty==0 then remove)
        -> H.MinHeap (Edge SomeSellOrder)
    replaceSubtractedOrder existingEdgeHeap (Edge newOrder _) =
        -- TODO: move normFac to 'BuyPath'
        -- Ignore normFac of "SomeSellOrder" argument (-1.0) (placeholder value)
        let Just (Edge _ normFac, remainingOrdersHeap) = H.view existingEdgeHeap
        in case soQty newOrder of
            0 -> remainingOrdersHeap
            _ -> H.insert (Edge newOrder normFac) remainingOrdersHeap
    src = fromString $ symbolVal (Proxy :: Proxy quote)
    dst = fromString $ symbolVal (Proxy :: Proxy base)


-- ^ subtract the quantity of the order with the smallest quantity
--    from all the other orders in the list.
--   the sequence of orders must be of the following form:
--      [ Order "BTC" "USD"
--      , Order "USD" "EUR"
--      , Order "EUR" "ETH"
--      , Order "ETH" "LOL"
--      ]
--   at least one of the orders will end up with zero quantity.
subtractMatchedQty
    :: NonEmpty (Edge SomeSellOrder)    -- ^ Order path/sequence
    -> ( NonEmpty (Edge SomeSellOrder)  -- ^ New orders (old orders with the matched order subtracted)
       , SomeSellOrder                  -- ^ Matched order
       )
subtractMatchedQty edges =
    Exchange.withSomeSellOrders someSellOrders $ \orders ->
        let maxOrder = Exchange.maxOrder orders
            newOrders = Exchange.minusQty orders (Exchange.oQty maxOrder)
            newOrderQtys = Exchange.asList (Exchange.rawQty . Exchange.oQty) newOrders
        in
            ( fmap (`Edge` normFac) $ NE.zipWith setQty someSellOrders (NE.fromList newOrderQtys)
            , Exchange.toSomeSellOrder maxOrder venues
            )
  where
    -- The venues moved through, separated by ","
    venues = T.concat . NE.toList . NE.intersperse "," $ NE.map soVenue someSellOrders
    someSellOrders = fmap getEdge edges
    normFac = getNormalizationFactor $ NE.head edges
    setQty someSellOrder qty = someSellOrder { soQty = qty }
