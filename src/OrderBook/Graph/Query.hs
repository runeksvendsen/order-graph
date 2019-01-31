{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
module OrderBook.Graph.Query
( Currency
, SomeSellOrder'(..)
, SomeSellOrder, SomeBuyOrder
, OrderGraph
, GraphM
, MeasuredPath(MeasuredPath), mpDist, mpOrders
, query
, subtractMatchedQty
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types
import qualified Data.Graph.Types                           as G
import qualified Data.Graph.Immutable                       as GI
import qualified Data.List.NonEmpty                         as NE



-- | Source: https://github.com/andrewthad/impure-containers/issues/8#issuecomment-454373569
data MeasuredPath = MeasuredPath
    { mpDist    :: Double
    , mpOrders'  :: [SomeSellOrder]
    } deriving Eq

mpOrders :: MeasuredPath -> [SomeSellOrder]
mpOrders = reverse . mpOrders'

instance Ord MeasuredPath where
    compare (MeasuredPath len1 vs1) (MeasuredPath len2 vs2) =
        compare len1 len2 <> compare vs1 vs2
instance Semigroup MeasuredPath where
    (<>) = min
instance Monoid MeasuredPath where
    mempty = MeasuredPath (1/0) []
instance Show MeasuredPath where
    show (MeasuredPath len orders) =
        "{ len: " ++ show len ++ ", orders: " ++ show (reverse orders) ++ " }"

-- ^ Find the lowest price going from one 'Currency' to another
query
    :: G.Graph g (Edge SomeSellOrder) Currency      -- ^ Graph with lowest-price edges/orders
    -> Currency                                     -- ^ Start vertex/currency
    -> Currency                                     -- ^ Start vertex/currency
    -> MeasuredPath
query graph start end =
    let pathGraph = GI.dijkstra combine (MeasuredPath 1 []) [startVertex] graph
        Just endVertex = GI.lookupVertex end graph
    in GI.atVertex endVertex pathGraph
  where
    Just startVertex = GI.lookupVertex start graph
    combine     -- Source: https://github.com/andrewthad/impure-containers/issues/8#issuecomment-454373569
        :: Currency             -- src
        -> Currency             -- dst
        -> MeasuredPath
        -> Edge SomeSellOrder
        -> MeasuredPath
    combine _src _ (MeasuredPath len orders) orderEdge@(Edge sellOrder _) =
        MeasuredPath (len * weight orderEdge) (sellOrder : orders)


-- ^ subtract the quantity of the order with the smallest quantity
--    from all the other orders in the list.
--   the sequence of orders must be of the following form:
--    [Order "BTC" "USD", ?
--   at least one of the orders will end up with zero quantity.
subtractMatchedQty :: NonEmpty SomeSellOrder -> [SomeSellOrder]
subtractMatchedQty orders = fst $
    foldl subtractExchange ([], matchedOrder orders) orders
  where
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
        | soQty so2 > soQty so1 = error $ "qty2 > qty1. (qty1,qty2) = " ++ show (soQty so1,soQty so2)
        | soBase so1 /= soBase so2 = error $ "base1 /= base2. (base1,base2) = " ++ show (soBase so1, soBase so2)
        | otherwise = so1 { soQty = soQty so1 - soQty so2 }
