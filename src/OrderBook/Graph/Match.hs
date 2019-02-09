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
    let orders = fromMaybe noPathError . Query.mpOrders $ findPath graph
        noPathError = error $ "no path: " ++ show (src,dst)
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
        else matchR (matchedOrdersR ++ fmap getEdge matchedEdges) mGraph bo
  where
    -- ^ TODO: "existing" and "new" in the right order?
    subtractMatchedOrder
        :: H.MinHeap (Edge SomeSellOrder)
        -> Edge SomeSellOrder
        -> H.MinHeap (Edge SomeSellOrder)
    subtractMatchedOrder existingEdgeHeap (Edge matchingOrder _) =
        -- Ignore incoming (-1.0) "matchedOrder" normFac (placeholder value)
        -- TODO: move normFac to 'MeasuredPath'
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
subtractMatchedQty :: NonEmpty (Edge SomeSellOrder) -> [Edge SomeSellOrder]
subtractMatchedQty edges = map (`Edge` normFac) $ fst $
    foldl subtractExchange ([], matchedOrder orders) orders
  where
    orders = fmap getEdge edges
    normFac = getNormalizationFactor $ NE.head edges
    subtractExchange
        :: ([SomeSellOrder], SomeSellOrder)
        -> SomeSellOrder
        -> ([SomeSellOrder], SomeSellOrder)
    subtractExchange (orderList, toSubtract) order =
        let subtractedOrder = order `minusQtyOf` toSubtract
            nextToSubtract = toSubtract
                { soQty = soPrice toSubtract * soQty toSubtract
                , soBase = soQuote toSubtract
                }
        in (subtractedOrder : orderList, nextToSubtract)
    minusQtyOf :: SomeSellOrder -> SomeSellOrder -> SomeSellOrder
    minusQtyOf so1 so2
        | soQty so2 > soQty so1 = error $ "qty2 > qty1:" ++ pp (so1, so2)
        | soBase so1 /= soBase so2 = error $ "base1 /= base2:" ++ pp (so1, so2)
        | otherwise = so1 { soQty = soQty so1 - soQty so2 }

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

