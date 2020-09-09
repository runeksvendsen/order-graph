{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Match
( match
, arbitrages
, BuyOrder
, BuyOrder'(..)
, unlimited
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Match.Types
import           OrderBook.Graph.Build                      (Currency,  SomeSellOrder
                                                            , SomeSellOrder'(..), OrderBook
                                                            )
import           OrderBook.Graph.Types.Path                 as Path
import           OrderBook.Graph.Types.SortedOrders         (toSortedOrders, fromSortedOrders)
import qualified OrderBook.Graph.Build                      as B
import qualified OrderBook.Graph.Query                      as Query
import qualified OrderBook.Graph.Exchange                   as Exchange

import qualified Data.Graph.Digraph                         as DG
import qualified Data.Graph.BellmanFord                     as BF
import qualified Data.List.NonEmpty                         as NE
import qualified Data.Text                                  as T
import           Unsafe.Coerce                              (unsafeCoerce)


match
    :: forall s base quote.
       (KnownSymbol base, KnownSymbol quote)
    => BuyOrder base quote
    -> Query.BuyGraphM s [BuyPathR]
match bo =
    reverse . mrOrders <$> queryUpdateGraph bo (Query.buyPath src dst)
  where
    src = fromString $ symbolVal (Proxy :: Proxy quote)
    dst = fromString $ symbolVal (Proxy :: Proxy base)

-- | Find all arbitrages that are reachable from the specified 'Currency'.
arbitrages
    :: Currency -- ^ Start here
    -- ^ Returns the arbitrages plus a graph from which these arbitrages have been removed
    -> Query.ArbGraphM s (B.SellOrderGraph s "buy", [BuyPathR])
arbitrages src = do
    mr <- queryUpdateGraph unlimited (Query.arbitrage src)
    g <- BF.getGraph
    return (unsafeCoerce g, reverse $ mrOrders mr)

queryUpdateGraph
    :: BuyOrder base quote
    -> Query.AnyGraphM s kind (Maybe Query.ShortestPath)
    -> Query.AnyGraphM s kind MatchResult
queryUpdateGraph bo queryGraph =
    {-# SCC queryUpdateGraph #-} go empty
  where
    go mr = do
        buyPathM <- if not (orderFilled bo mr) then queryGraph else return Nothing
        case buyPathM of
            Nothing -> return mr
            Just sp -> do
                let (edgeOrderList, buyPath) = subtractMatchedQty sp
                forM_ edgeOrderList (uncurry updateGraphEdge)
                go (addOrder mr buyPath)

updateGraphEdge
    :: DG.IdxEdge Currency B.SortedOrders
    -> SomeSellOrder                -- ^ Updated top order
    -> Query.AnyGraphM s kind ()
updateGraphEdge orderListEdge newTopOrder = do
    let newOrderListM orderList = replaceSubtractedOrder orderList newTopOrder
    graph <- BF.getGraph -- HACK: Just to make things work for now (before we improve the algorithm)
    -- TODO: use BellmanFord "remove/updateEdge"
    case newOrderListM (DG.eMeta orderListEdge) >>= \so -> Just (fmap (const so) orderListEdge) of
        Nothing           -> lift $ DG.removeEdge graph (B.Tagged <$> fromSortedOrders orderListEdge)
        Just newOrderList -> lift $ DG.updateEdge graph (B.Tagged <$> fromSortedOrders newOrderList)

replaceSubtractedOrder
    :: B.SortedOrders       -- List of sorted orders, with old order at the head
    -> SomeSellOrder        -- New head order (if qty==0 then remove)
    -> Maybe B.SortedOrders -- List of sorted orders with top orders replaced/removed
replaceSubtractedOrder sortedOrders newOrder =
    -- Assert that the first order of "sortedOrders" and "newOrder"
    --  are the same except for quantity
    assert (setQty 0 (B.first sortedOrders) == setQty 0 newOrder) $
    B.replaceHead sortedOrders $
        case soQty newOrder of
            0 -> Nothing
            _ -> Just newOrder

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
    :: Query.ShortestPath -- ^ Order path/sequence
    -- | fst: list of (existing graph edge, edge's new top order)
    --   snd: Matched buy path
    -> ( NonEmpty (B.SortedOrders, SomeSellOrder)
       , BuyPathR
       )
subtractMatchedQty sp =
    Exchange.withSomeSellOrders sp $ \revSortedOrders orders ->
        let maxOrder = Exchange.maxOrder orders
            newOrders = Exchange.minusQty orders (Exchange.oQty maxOrder)
            newOrderQtys = Exchange.asList (Exchange.rawQty . Exchange.oQty) newOrders
            newEdges = NE.zipWith setQty (NE.fromList newOrderQtys) (fmap B.first revSortedOrders)
        in
            ( NE.zip revSortedOrders newEdges
            , toPath maxOrder revSortedOrders
            )

-- | Helper function
setQty
    :: numType
    -> SomeSellOrder' numType
    -> SomeSellOrder' numType
setQty qty someSellOrder =
    someSellOrder { soQty = qty }
