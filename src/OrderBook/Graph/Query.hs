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
, SomeSellOrder
, BuyPath(BuyPath), mpPrice, mpOrders, mpPath
, query
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types
import qualified Data.Graph.Types                           as G
import qualified Data.Graph.Immutable                       as GI


-- | Source: https://github.com/andrewthad/impure-containers/issues/8#issuecomment-454373569
data BuyPath = BuyPath
    { mpPrice   :: Rational
    , mpOrders  :: Maybe (NonEmpty (Edge SomeSellOrder))
    , mpPath    :: [(Currency, Currency)]
    } deriving (Eq, Generic)

instance Ord BuyPath where
    compare (BuyPath price1 _ _) (BuyPath price2 _ _) =
        compare price1 price2

instance Semigroup BuyPath where
    (<>) = min
instance Monoid BuyPath where
    mempty = BuyPath largeRational Nothing []

instance PrettyVal BuyPath

-- ^ Find the lowest price going from one 'Currency' to another
query
    :: G.Graph g (Edge SomeSellOrder) Currency      -- ^ Graph with lowest-price edges/orders
    -> Currency                                     -- ^ Start vertex/currency
    -> Currency                                     -- ^ Start vertex/currency
    -> BuyPath        -- ^ Lowest-price path ('Nothing' if no path exists at all)
query graph start end =
    let pathGraph = GI.dijkstra multiplyWeight (BuyPath 1 Nothing []) [startVertex] graph
        Just endVertex = GI.lookupVertex end graph
    in GI.atVertex endVertex pathGraph
  where
    Just startVertex = GI.lookupVertex start graph
    multiplyWeight
        :: Currency             -- src
        -> Currency             -- dst
        -> BuyPath
        -> Edge SomeSellOrder
        -> BuyPath
    multiplyWeight _src _dst (BuyPath len edges pathL) orderEdge =
        let newPathL = (_src, _dst) : pathL
            assertMinOne num
                | num < 1 = error $ "multiplyWeight: num < 1: " ++ show num
                | otherwise = num
        in BuyPath (len * assertMinOne (weight orderEdge))
                   (addEdge orderEdge edges)
                   newPathL
    addEdge :: Edge SomeSellOrder
            -> Maybe (NonEmpty (Edge SomeSellOrder))
            -> Maybe (NonEmpty (Edge SomeSellOrder))
    addEdge edge Nothing      = Just $ edge :| []
    addEdge edge (Just edges) = Just $ edge `cons` edges
