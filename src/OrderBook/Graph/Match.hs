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
-- DEBUG
import qualified OrderBook.Graph.Internal.Util              as Util
import  System.IO.Unsafe
import  Debug.Trace
import qualified Data.Graph.Mutable                         as GM


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
    :: forall g base quote.
       (KnownSymbol base, KnownSymbol quote)
    => G.MGraph (PrimState IO) g Build.SellOrderHeap Currency
    -> BuyOrder base quote
    -> IO [SomeSellOrder]
match g bo = do
    dummyGraph <- Build.derive g
    fmap reverse $ matchR [] g bo (dummyGraph, mempty)

-- |
debugPrint
    :: (G.Graph g (Edge SomeSellOrder) Currency, Query.BuyPath)     -- ^ Previous graph/path
    -> (G.Graph g (Edge SomeSellOrder) Currency, Query.BuyPath)     -- ^ Current graph/path
    -> IO ()
debugPrint (prevGraph, prevPath) (currGraph, currPath) = do
    void $ GI.create $ \mGraphA -> do
        prevSubGraph <- Util.subgraph mGraphA prevGraph (Query.mpPath prevPath)
        void $ GI.create $ \mGraphB -> do
            currSubGraph <- Util.subgraph mGraphB currGraph (Query.mpPath currPath)
            void $ GI.create $ \mGraphUnion -> do
                finalGraph <- Util.union mGraphUnion prevSubGraph currSubGraph
                GI.traverseEdges_ printEdge finalGraph
  where
    printEdge vFrom vTo from to edge =
        putStrLn $ printf "%s\t->\t%s\t%s" (show from) (show to) (show edge)

matchR
    :: forall g base quote.
       (KnownSymbol base, KnownSymbol quote)
    => [SomeSellOrder]
    -> G.MGraph (PrimState IO) g Build.SellOrderHeap Currency
    -> BuyOrder base quote
    -> (G.Graph g (Edge SomeSellOrder) Currency, Query.BuyPath)
    -> IO [SomeSellOrder]
matchR matchedOrdersR mGraph bo@BuyOrder'{..} prevGraphPath = do
    graph <- Build.derive mGraph
    let getVertex v = justOrFail ("Vertex not found", v) (GI.lookupVertex v graph)
    let buyPath = Query.query graph src dst
    case Query.mpOrders buyPath of
        Nothing        -> return matchedOrdersR
        Just orderPath -> do
            let (newEdges, matchedOrder) = subtractMatchedQty orderPath
            forM_ newEdges (updateEdgeHeap getVertex)

            let peek :: [a] -> [a]
                peek []           = []
                peek (x:[])       = [x]
                peek (x1:x2:[])   = [x1,x2]
                peek (x1:x2:x3:_) = [x1,x2,x3]

            let checkOrders :: SomeSellOrder -> [SomeSellOrder] -> [String]
                checkOrders _ [] = []
                checkOrders order prevOrders@(prevOrder:_) =
                    if not (soPrice order >= soPrice prevOrder)
                        then
                            [   ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                , "Orders not sorted:"
                                , pp order
                                , "Previous orders:"
                                ] ++ map pp (peek prevOrders)
                                  ++ ["Order path:", pp orderPath]

                        else []
            let debug = checkOrders matchedOrder matchedOrdersR
            -- putStrLn $ unlines ["Order path:", pp orderPath, ""]
            let nextGraphPath = (graph, buyPath)
            when (debug /= []) $ do
                putStrLn $ unlines debug
                debugPrint prevGraphPath nextGraphPath
                putStrLn "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
                putStrLn ""
            matchR (matchedOrder : matchedOrdersR) mGraph bo nextGraphPath
            -- TODO: check BuyOrder quantity and maxPrice
  where
    updateEdgeHeap
        :: (Currency -> G.Vertex g)     -- ^ Get a vertex from a vertex label
        -> Edge SomeSellOrder           -- ^ Updated top order
        -> IO ()
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
