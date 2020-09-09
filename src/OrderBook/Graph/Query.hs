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
, ShortestPath(..)
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

data ShortestPath = ShortestPath
    { bpOrders  :: NonEmpty B.SortedOrders
    } deriving (Eq, Generic)

-- ^ Find the lowest price buy path going from one 'Currency' to another
buyPath
    :: Currency                     -- ^ Start vertex/currency
    -> Currency                     -- ^ End vertex/currency
    -> BuyGraphM s g (Maybe ShortestPath)         -- ^ Lowest-price path ('Nothing' if no path exists)
buyPath start end = do
    BF.bellmanFord start
    pathM <- BF.pathTo end
    return $ ShortestPath . NE.fromList <$> fmap B.unTagged <$> pathM

-- | find an arbitrage opportunity
arbitrage
    :: Currency                     -- ^ Start vertex/currency
    -> ArbGraphM s g (Maybe ShortestPath)         -- ^ Arbitrage path ('Nothing' if no arbitrage exists)
arbitrage start = do
    BF.bellmanFord start
    pathM <- BF.negativeCycle
    return $ ShortestPath <$> fmap B.unTagged <$> pathM
