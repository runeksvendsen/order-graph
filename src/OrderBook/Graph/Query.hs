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
)
where

import           OrderBook.Graph.Internal.Prelude
import qualified OrderBook.Graph.Build                      as B
import           OrderBook.Graph.Types
import qualified Data.Graph.BellmanFord                     as BF
import           Control.Monad.ST                           (ST)
import qualified Data.List.NonEmpty                         as NE


data BuyPath = BuyPath
    { bpOrders  :: NonEmpty B.SortedOrders
    } deriving (Eq, Generic)

-- ^ Find the lowest price buy path going from one 'Currency' to another
buyPath
    :: B.SellOrderGraph s g "buy"   -- ^ Graph without negative cycles
    -> Currency                     -- ^ Start vertex/currency
    -> Currency                     -- ^ End vertex/currency
    -> ST s (Maybe BuyPath)         -- ^ Lowest-price path ('Nothing' if no path exists)
buyPath graph start end = do
    state <- BF.bellmanFord graph start multiplyWeight
    pathM <- BF.pathTo graph state end
    return $ BuyPath . NE.fromList <$> fmap B.unTagged <$> pathM
  where
    multiplyWeight
        :: Double
        -> B.Tagged "buy" B.SortedOrders
        -> Double
    multiplyWeight weight' edge = weight' * BF.weight edge

-- | find an arbitrage opportunity
arbitrage
    :: B.SellOrderGraph s g "arb"   -- ^ Graph which may contain negative cycles
    -> Currency                     -- ^ Start vertex/currency
    -> ST s (Maybe BuyPath)         -- ^ Arbitrage path ('Nothing' if no arbitrage exists)
arbitrage graph start = do
    state <- BF.bellmanFord graph start sumWeight
    pathM <- BF.negativeCycle state
    return $ BuyPath <$> fmap B.unTagged <$> pathM
  where
    sumWeight
        :: Double
        -> B.Tagged "arb" B.SortedOrders
        -> Double
    sumWeight weight' edge = weight' + BF.weight edge
