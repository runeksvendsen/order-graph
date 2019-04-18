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
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Build                      ( SomeSellOrder
                                                            , SomeSellOrder'(..)
                                                            )
import qualified OrderBook.Graph.Build                      as B
import qualified OrderBook.Graph.Query                      as Query
import qualified OrderBook.Graph.Exchange                   as Exchange

import qualified Data.Graph.Digraph                         as DG
import qualified Data.Graph.BellmanFord                     as BF
import qualified Data.List.NonEmpty                         as NE
import qualified Data.Text                                  as T
import           Unsafe.Coerce                              (unsafeCoerce)


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
    :: forall s g base quote.
       (KnownSymbol base, KnownSymbol quote)
    => BuyOrder base quote
    -> Query.BuyGraphM s g [SomeSellOrder]
match bo =
    reverse <$> queryUpdateGraph bo (Query.buyPath src dst)
  where
    src = fromString $ symbolVal (Proxy :: Proxy quote)
    dst = fromString $ symbolVal (Proxy :: Proxy base)

arbitrages
    :: forall s g base quote.
       (KnownSymbol base, KnownSymbol quote)
    => BuyOrder base quote
    -> Query.ArbGraphM s g (B.SellOrderGraph s g "buy", [SomeSellOrder])
arbitrages bo = do
    orders <- queryUpdateGraph bo (Query.arbitrage src)
    g <- BF.getGraph
    return (unsafeCoerce g, reverse orders)
  where
    src = fromString $ symbolVal (Proxy :: Proxy quote)

queryUpdateGraph
    :: forall s g base quote kind.
       (KnownSymbol base, KnownSymbol quote)
    => BuyOrder base quote
    -> Query.AnyGraphM s g kind (Maybe Query.BuyPath)
    -> Query.AnyGraphM s g kind [SomeSellOrder]
queryUpdateGraph bo queryGraph =
    go []
  where
    go accum = do
        buyPathM <- queryGraph
        case buyPathM of
            Nothing -> return accum
            Just (Query.BuyPath orderPath) -> do
                -- The buyer moves in the opposite direction of the seller.
                --   So when composing sell orders we need them to be in reverse order.
                let revOrderPath = NE.reverse orderPath
                    (newEdges, matchedOrder) = subtractMatchedQty revOrderPath
                forM_ (NE.zip revOrderPath newEdges) (uncurry updateGraphEdge)
                go (matchedOrder : accum)

updateGraphEdge
    :: B.SortedOrders
    -> SomeSellOrder                -- ^ Updated top order
    -> Query.AnyGraphM s g kind ()
updateGraphEdge orderList newTopOrder = do
    let newOrderListM = replaceSubtractedOrder orderList newTopOrder
    graph <- BF.getGraph -- HACK: Just to make things work for now (before we improve the algorithm)
    -- TODO: use BellmanFord "remove/updateEdge"
    case newOrderListM of
        Nothing           -> DG.removeEdge graph (B.Tagged orderList)
        Just newOrderList -> DG.insertEdge graph (B.Tagged newOrderList)

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
    :: NonEmpty B.SortedOrders   -- ^ Order path/sequence
    -> ( NonEmpty SomeSellOrder  -- ^ New orders (old orders with the matched order subtracted)
       , SomeSellOrder           -- ^ Matched order
       )
subtractMatchedQty sortedOrders =
    Exchange.withSomeSellOrders someSellOrders $ \orders ->
        let maxOrder = Exchange.maxOrder orders
            newOrders = Exchange.minusQty orders (Exchange.oQty maxOrder)
            newOrderQtys = Exchange.asList (Exchange.rawQty . Exchange.oQty) newOrders
        in
            ( NE.zipWith setQty (NE.fromList newOrderQtys) someSellOrders
            , Exchange.toSomeSellOrder maxOrder venues
            )
  where
    someSellOrders = fmap B.first sortedOrders
    -- The venues moved through, separated by ","
    venues = T.concat . NE.toList . NE.intersperse "," $ NE.map soVenue someSellOrders

-- | Helper function
setQty
    :: numType
    -> SomeSellOrder' numType
    -> SomeSellOrder' numType
setQty qty someSellOrder =
    someSellOrder { soQty = qty }
