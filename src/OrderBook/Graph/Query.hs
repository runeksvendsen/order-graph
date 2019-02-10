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
, BuyPath(BuyPath), mpPrice, mpOrders
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
data BuyPath = BuyPath
    { mpPrice   :: Rational
    , mpOrders  :: Maybe (NonEmpty (Edge SomeSellOrder))
    } deriving (Eq, Generic)

instance Ord BuyPath where
    compare (BuyPath price1 vs1) (BuyPath price2 vs2) =
        compare price1 price2 <> compare vs1 vs2
        -- prefer:
        --  * lowest price (1st priority)
        --  * fewest orders (2nd priority) TODO: fewest number of orders preferred?
instance Semigroup BuyPath where
    (<>) = min
instance Monoid BuyPath where
    mempty = BuyPath largeRational Nothing

instance PrettyVal BuyPath

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
    -> Maybe (NonEmpty (Edge SomeSellOrder))        -- ^ Lowest-price path ('Nothing' if no path exists at all)
query graph start end =
    let pathGraph = GI.dijkstra multiplyWeight (BuyPath 1 Nothing) [startVertex] graph
        Just endVertex = GI.lookupVertex end graph
    in mpOrders $ GI.atVertex endVertex pathGraph
  where
    Just startVertex = GI.lookupVertex start graph
    multiplyWeight
        :: Currency             -- src
        -> Currency             -- dst
        -> BuyPath
        -> Edge SomeSellOrder
        -> BuyPath
    multiplyWeight _src _ (BuyPath len edges) orderEdge =
        BuyPath (len * weight orderEdge) (addEdge orderEdge edges)
    addEdge :: Edge SomeSellOrder
            -> Maybe (NonEmpty (Edge SomeSellOrder))
            -> Maybe (NonEmpty (Edge SomeSellOrder))
    addEdge edge Nothing      = Just $ edge :| []
    addEdge edge (Just edges) = Just $ edge `cons` edges
