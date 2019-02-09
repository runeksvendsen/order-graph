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
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types
import           OrderBook.Graph.Exchange
import qualified Data.Graph.Types                           as G
import qualified Data.Graph.Immutable                       as GI
import           Data.Thrist                                (Thrist(..))


type GraphPath = Thrist Order

-- | Source: https://github.com/andrewthad/impure-containers/issues/8#issuecomment-454373569
data MeasuredPath = MeasuredPath
    { mpDist    :: Double
    , mpOrders  :: Maybe (NonEmpty (Edge SomeSellOrder))
    } deriving (Eq, Generic)

instance Ord MeasuredPath where
    compare (MeasuredPath len1 vs1) (MeasuredPath len2 vs2) =
        compare len1 len2 <> compare vs1 vs2
instance Semigroup MeasuredPath where
    (<>) = min
instance Monoid MeasuredPath where
    mempty = MeasuredPath (1/0) Nothing

instance PrettyVal MeasuredPath

-- query
--     :: forall g src dst.
--        (KnownSymbol src, KnownSymbol dst)
--     => G.Graph g (Edge SomeSellOrder) Currency      -- ^ Graph with lowest-price edges/orders
--     -> GraphPath src dst
-- query g =
--     convertOrFail . map getEdge . mpOrders $ query' g src dst
--   where
--     src = fromString $ symbolVal (Proxy :: Proxy src)
--     dst = fromString $ symbolVal (Proxy :: Proxy dst)
 

-- ^ Find the lowest price going from one 'Currency' to another
query
    :: G.Graph g (Edge SomeSellOrder) Currency      -- ^ Graph with lowest-price edges/orders
    -> Currency                                     -- ^ Start vertex/currency
    -> Currency                                     -- ^ Start vertex/currency
    -> MeasuredPath
query graph start end =
    let pathGraph = GI.dijkstra combine (MeasuredPath 1 Nothing) [startVertex] graph
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
    combine _src _ (MeasuredPath len edges) orderEdge =
        MeasuredPath (len * weight orderEdge) (addEdge orderEdge edges)
    addEdge :: Edge SomeSellOrder 
            -> Maybe (NonEmpty (Edge SomeSellOrder))
            -> Maybe (NonEmpty (Edge SomeSellOrder))
    addEdge edge Nothing      = Just $ edge :| []
    addEdge edge (Just edges) = Just $ edge `cons` edges
