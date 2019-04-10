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
import qualified Data.Graph.BellmanFord                     as BF
import qualified Data.List.NonEmpty                         as NE


type AnyGraphM s g kind = BF.BF s g (B.Tagged kind B.SortedOrders) Currency
type ArbGraphM s g = BF.BF s g (B.Tagged "arb" B.SortedOrders) Currency
type BuyGraphM s g = BF.BF s g (B.Tagged "buy" B.SortedOrders) Currency

data BuyPath = BuyPath
    { bpOrders  :: NonEmpty B.SortedOrders
    } deriving (Eq, Generic)

-- ^ Find the lowest price buy path going from one 'Currency' to another
buyPath
    :: Currency                     -- ^ Start vertex/currency
    -> Currency                     -- ^ End vertex/currency
    -> BuyGraphM s g (Maybe BuyPath)         -- ^ Lowest-price path ('Nothing' if no path exists)
buyPath start end = do
    BF.bellmanFord start
    pathM <- BF.pathTo end
    return $ BuyPath . NE.fromList <$> fmap B.unTagged <$> pathM
  where
    multiplyWeight
        :: Double
        -> B.Tagged "buy" B.SortedOrders
        -> Double
    multiplyWeight weight' edge = weight' * BF.weight edge

-- | find an arbitrage opportunity
arbitrage
    :: Currency                     -- ^ Start vertex/currency
    -> ArbGraphM s g (Maybe BuyPath)         -- ^ Arbitrage path ('Nothing' if no arbitrage exists)
arbitrage start = do
    BF.bellmanFord start
    pathM <- BF.negativeCycle
    return $ BuyPath <$> fmap B.unTagged <$> pathM
  where
    sumWeight
        :: Double
        -> B.Tagged "arb" B.SortedOrders
        -> Double
    sumWeight weight' edge = weight' + BF.weight edge
