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


match
    :: forall s g base quote.
       (KnownSymbol base, KnownSymbol quote)
    => BuyOrder base quote
    -> Query.BuyGraphM s g [SomeSellOrder]
match bo =
    reverse . mrOrders <$> queryUpdateGraph bo (Query.buyPath src dst)
  where
    src = fromString $ symbolVal (Proxy :: Proxy quote)
    dst = fromString $ symbolVal (Proxy :: Proxy base)

-- | NB: 'BuyOrder' is only used to specify 'src' currency.
--   No other information from the 'BuyOrder' is used.
arbitrages
    :: forall s g base quote.
       (KnownSymbol base, KnownSymbol quote)
    => BuyOrder base quote
    -> Query.ArbGraphM s g (B.SellOrderGraph s g "buy", [SomeSellOrder])
arbitrages bo = do
    mr <- queryUpdateGraph unlimitedBuyOrder (Query.arbitrage src)
    g <- BF.getGraph
    return (unsafeCoerce g, reverse $ mrOrders mr)
  where
    src = fromString $ symbolVal (Proxy :: Proxy quote)
    unlimitedBuyOrder :: BuyOrder base quote
    unlimitedBuyOrder = unlimited

queryUpdateGraph
    :: forall s g base quote kind.
       (KnownSymbol base, KnownSymbol quote)
    => BuyOrder base quote
    -> Query.AnyGraphM s g kind (Maybe Query.BuyPath)
    -> Query.AnyGraphM s g kind MatchResult
queryUpdateGraph bo queryGraph =
    {-# SCC queryUpdateGraph #-} go empty
  where
    go mr = do
        buyPathM <- if not (orderFilled bo mr) then queryGraph else return Nothing
        case buyPathM of
            Nothing -> return mr
            Just (Query.BuyPath orderPath) -> do
                let reverseOrderPath = NE.reverse orderPath
                let (newEdges, matchedOrder) = subtractMatchedQty reverseOrderPath
                forM_ (NE.zip reverseOrderPath newEdges) (uncurry updateGraphEdge)
                go (addOrder mr matchedOrder)

updateGraphEdge
    :: B.SortedOrders
    -> SomeSellOrder                -- ^ Updated top order
    -> Query.AnyGraphM s g kind ()
updateGraphEdge orderList newTopOrder = do
    let newOrderListM = replaceSubtractedOrder orderList newTopOrder
    graph <- BF.getGraph -- HACK: Just to make things work for now (before we improve the algorithm)
    -- TODO: use BellmanFord "remove/updateEdge"
    case newOrderListM of
        Nothing           -> lift $ DG.removeEdge graph (B.Tagged orderList)
        Just newOrderList -> lift $ DG.updateEdge graph (B.Tagged newOrderList)

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
    :: NonEmpty B.SortedOrders -- ^ Order path/sequence
    -- | fst: New orders (old orders with the matched order subtracted)
    --   snd: Matched order
    -> ( NonEmpty SomeSellOrder
       , SomeSellOrder
       )
subtractMatchedQty sortedOrders =
    Exchange.withSomeSellOrders someSellOrders $ \orders ->
        let maxOrder = Exchange.maxOrder orders
            newOrders = Exchange.minusQty orders (Exchange.oQty maxOrder)
            newOrderQtys = Exchange.asList (Exchange.rawQty . Exchange.oQty) newOrders
        in
            ( NE.zipWith setQty (NE.fromList newOrderQtys) someSellOrders
            , Exchange.toSomeSellOrder maxOrder (T.concat cryptoPath)
            )
  where
    -- The path moved through from the buyer's perspective.
    -- In the above example the buyer moves from LOL to BTC (quote to base).
    -- The seller moves in the opposite direction: from BTC to LOL (base to quote).
    cryptoPath =
        let (first NE.:| rest) = NE.reverse someSellOrders
            arrowToBase so = " --" <> soVenue so <> "--> " <> toS (soBase so)
        in toS (soQuote first) : map arrowToBase (first : rest)
    someSellOrders = fmap B.first sortedOrders

-- | Helper function
setQty
    :: numType
    -> SomeSellOrder' numType
    -> SomeSellOrder' numType
setQty qty someSellOrder =
    someSellOrder { soQty = qty }
