{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module OrderBook.Graph.Types.SomeSellOrder
( DG.DirectedEdge(..)
, GE.WeightedEdge(..)
, Currency
, SomeSellOrder'(..)
, SomeSellOrder
, NumType
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types.Currency

import qualified Data.Graph.Digraph                         as DG
import qualified Data.Graph.Edge                            as GE
import qualified Data.Text                                  as T


-- | A sell order.
--   An offer to exchange 'soQty' of 'soBase' for 'soQuote',
--    at 'soPrice' (which has unit: 'soQuote' per 'soBase').
data SomeSellOrder' numType =
    SomeSellOrder'
    { soPrice :: numType
    , soQty   :: numType
    , soBase  :: Currency
    , soQuote :: Currency
    , soVenue :: T.Text
    } deriving (Eq, Read, Ord, Functor, Generic)

type NumType = Rational
type SomeSellOrder = SomeSellOrder' NumType

instance Real numType => Show (SomeSellOrder' numType) where
    show SomeSellOrder'{..} =
        printf "Order { %s qty=%f price=%f %s/%s }"
            soVenue
            (realToFrac soQty :: Double)
            (realToFrac soPrice :: Double)
            (toS soBase :: String)
            (toS soQuote :: String)

instance NFData numType => NFData (SomeSellOrder' numType)
instance PrettyVal (SomeSellOrder' Double)

instance PrettyVal SomeSellOrder where
    prettyVal sso =
        prettyVal (fmap realToFrac sso :: SomeSellOrder' Double)

instance DG.DirectedEdge (SomeSellOrder' numType) Currency where
    -- We move in the opposite direction of the seller
    fromNode = soQuote
    toNode = soBase
    multiKey = soVenue

instance GE.WeightedEdge (SomeSellOrder' numType) Currency numType where
    weight = soPrice
