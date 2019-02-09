{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module OrderBook.Graph.Match
( match
, BuyOrder
, BuyOrder'(..)
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types                      ( IsEdge(..), Edge(..), Currency
                                                            , matchedOrder
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


-- | "base" and "quote" are destination and source currencies, respectively
data BuyOrder' numTyp (base :: Symbol) (quote :: Symbol) = BuyOrder'
    { boQuantity        :: numTyp
    , boMaxPrice        :: Maybe numTyp
      -- ^ (TODO: IGNORED FOR NOW) maximum price
    , boMaxSlippage     :: Maybe numTyp
      -- ^ (per-market) maximum percentage difference
      -- between price of first and last matched order

    }

type BuyOrder = BuyOrder' Double



match
    :: forall m g base quote.
       (PrimMonad m, KnownSymbol base, KnownSymbol quote)
    => G.MGraph (PrimState m) g Build.SellOrderHeap Currency
    -> BuyOrder base quote
    -> m [SomeSellOrder]
match = matchR []

matchR
    :: forall m g base quote.
       (PrimMonad m, KnownSymbol base, KnownSymbol quote)
    => [SomeSellOrder]
    -> G.MGraph (PrimState m) g Build.SellOrderHeap Currency
    -> BuyOrder base quote
    -> m [SomeSellOrder]
matchR matchedOrdersR mGraph bo@BuyOrder'{..} = do
    graph <- Build.derive mGraph
    let orders = justOrFail ("no path", (src,dst)) $ findPath graph
        matchedEdges = subtractMatchedQty orders
        getVertex v = justOrFail ("Vertex not found", v) (GI.lookupVertex v graph)
    forM_ matchedEdges $ \matchedEdge -> do
            let fromLabel = fromNode matchedEdge
                toLabel = toNode matchedEdge
                from = getVertex fromLabel
                to = getVertex toLabel
            -- The edge should always be present in the graph if it's returned by 'findPath'
            orderHeapM <- GM.lookupEdge mGraph from to
            let orderHeap = justOrFail ("Edge not in graph", (fromLabel,toLabel)) orderHeapM
            let updatedHeap = subtractMatchedOrder orderHeap matchedEdge
            case H.isEmpty updatedHeap of
                True ->  GM.removeEdge mGraph from to
                False -> GM.insertEdge mGraph from to updatedHeap
    -- TODO: check BuyOrder quantity and maxPrice
    if null orders
        then return matchedOrdersR
        else matchR (matchedOrdersR ++ NE.toList (fmap getEdge matchedEdges)) mGraph bo
  where
    -- ^ TODO: "existing" and "new" in the right order?
    subtractMatchedOrder
        :: H.MinHeap (Edge SomeSellOrder)
        -> Edge SomeSellOrder
        -> H.MinHeap (Edge SomeSellOrder)
    subtractMatchedOrder existingEdgeHeap (Edge matchingOrder _) =
        -- Ignore incoming (-1.0) "matchedOrder" normFac (placeholder value)
        -- TODO: move normFac to 'BuyPath'
        let Just (Edge topBookOrder normFac, remainingOrdersHeap) = H.view existingEdgeHeap
        in case topBookOrder `minus` matchingOrder of
            Just newOrder -> H.insert (Edge newOrder normFac) remainingOrdersHeap
            Nothing ->       remainingOrdersHeap
    findPath graph = Query.query graph src dst
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
subtractMatchedQty :: NonEmpty (Edge SomeSellOrder) -> NonEmpty (Edge SomeSellOrder)
subtractMatchedQty edges = fmap (`Edge` normFac) $
    Exchange.withSomeSellOrders someSellOrders $ \orders ->
        let qtyToRemove = Exchange.maxQty orders
            newOrders = Exchange.minusQty orders qtyToRemove
            newOrderQtys = Exchange.asList (Exchange.rawQty . Exchange.oQty) newOrders
        in NE.zipWith setQty someSellOrders (NE.fromList newOrderQtys)
  where
    someSellOrders = fmap getEdge edges
    normFac = getNormalizationFactor $ NE.head edges
    setQty someSellOrder qty = someSellOrder { soQty = qty }

minus
    :: SomeSellOrder        -- ^ Order from orderbook
    -> SomeSellOrder        -- ^ Matched order (quantity <= orderbook order quantity)
    -> Maybe SomeSellOrder  -- ^ Subtract matched order quantity from order book order quantity.
                            -- ^ 'Nothing' if quantity==0
minus topBookOrder matchedOrder
    -- bookQty-matchedQty == 0.0
    | bookQty == matchedQty =
        Nothing
    | compatibleOrders topBookOrder matchedOrder =
        Just $ fmap (const $ bookQty - matchedQty) topBookOrder
        -- TODO: can floating point errors make matchedQty > bookQty?
    | otherwise = error $ "BUG: minus. arguments: " ++ pp (topBookOrder, matchedOrder)
  where
    compatibleOrders :: SomeSellOrder -> SomeSellOrder -> Bool
    compatibleOrders o1 o2 =
        fmap (const @Double 0.0) o1 == fmap (const 0.0) o2
        && matchedQty <= bookQty
    bookQty = soQty topBookOrder
    matchedQty = soQty matchedOrder
