{-# LANGUAGE MultiParamTypeClasses #-}
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
, buyPath
, arbitrage
, BuyPath(..)
, BuyGraphM
, ArbGraphM
, AnyGraphM
)
where

import           OrderBook.Graph.Internal.Prelude
import qualified OrderBook.Graph.Build                      as B
import           OrderBook.Graph.Types
import           OrderBook.Graph.Types.SortedOrders (Tagged, CompactOrderList)

import qualified Data.Graph.Digraph                         as DG
import qualified Data.Graph.BellmanFord                     as BF
import qualified Data.List.NonEmpty                         as NE


type AnyGraphM s kind = BF.BF s Currency (B.Tagged kind CompactOrderList)
type ArbGraphM s = BF.BF s Currency (B.Tagged "arb" CompactOrderList)
type BuyGraphM s = BF.BF s Currency (B.Tagged "buy" CompactOrderList)

newtype BuyPath = BuyPath
    { bpOrders  :: NonEmpty (DG.IdxEdge Currency CompactOrderList)
    } deriving (Eq, Generic)

-- ^ Find the lowest price buy path going from one 'Currency' to another
buyPath
    :: Currency                     -- ^ Start vertex/currency
    -> Currency                     -- ^ End vertex/currency
    -> BuyGraphM s (Maybe BuyPath)         -- ^ Lowest-price path ('Nothing' if no path exists)
buyPath start end = do
    BF.bellmanFord start
    pathM <- BF.pathTo end
    return $ BuyPath . NE.fromList . removeTag <$> pathM

removeTag :: Functor f => f (DG.IdxEdge Currency (Tagged s CompactOrderList)) -> f (DG.IdxEdge Currency CompactOrderList)
removeTag path = fmap B.unTagged <$> path

-- | find an arbitrage opportunity
arbitrage
    :: Currency                     -- ^ Start vertex/currency
    -> ArbGraphM s (Maybe BuyPath)         -- ^ Arbitrage path ('Nothing' if no arbitrage exists)
arbitrage start = do
    BF.bellmanFord start
    pathM <- BF.negativeCycle
    return $ BuyPath . removeTag <$> pathM
