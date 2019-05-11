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
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types
import           OrderBook.Graph.Types.SortedOrders
import qualified OrderBook.Graph.Internal.Util              as Util
import           CryptoVenues.Types.ABook                   (ABook(ABook))

import qualified Data.Graph.Digraph                         as DG
import           Data.List                                  (groupBy, sortOn, sortBy)
import qualified Data.List.NonEmpty                         as NE


type SellOrderGraph s g kind = DG.Digraph s g (Tagged kind SortedOrders) Currency

-- ^ build a graph where each edge is a list of sorted sell orders
build
    :: (PrimMonad m)
    => SellOrderGraph (PrimState m) g "arb" -- ^ Empty graph
    -> [ABook]                              -- ^ Order books
    -> m ()
build mGraph orders = do
    forM_ (create orders) (DG.insertEdge mGraph . Tagged)

-- |
create
    :: [ABook]  -- ^ A bunch of order books
    -> [SortedOrders]
create =
        map (SortedOrders . NE.fromList . sortOn soPrice . doAssertions)
            . concat . map (concatListPairs . toOrders) . groupByMarket
  where
    concatListPairs :: ([[a]], [[a]]) -> [[a]]
    concatListPairs (listA, listB) = listA ++ listB
    toOrders :: [ABook] -> ([[SomeSellOrder]], [[SomeSellOrder]])
    toOrders aBookLst = foldl
        (\(accumSell, accumBuy) (sellOrders, buyOrders) ->
            (sellOrders : accumSell, buyOrders : accumBuy)
        )
        ([],[])
        (map (Util.withABook Util.toSellBuyOrders) aBookLst)
    doAssertions =
        assertSameBaseQuote . map assertPositivePrice
    assertSameBaseQuote lst =
        if all (sameBaseQuoteOrder (head lst)) lst
            then lst
            else error $ "SortedOrders with different base/quote: " ++ show lst
    sameBaseQuoteOrder o1 o2 =
        soBase o1 == soBase o2
        && soQuote o1 == soQuote o2
    assertPositivePrice order
        | soPrice order >= 0 = order
        | otherwise = error $ "negative-price order: " ++ show order
    groupByMarket :: [ABook] -> [[ABook]]
    groupByMarket = groupBy sameBaseQuote . sortOn Util.baseQuote
    sameBaseQuote :: ABook -> ABook -> Bool
    sameBaseQuote ob1 ob2 = Util.baseQuote ob1 == Util.baseQuote ob2
