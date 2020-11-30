{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OrderBook.Graph.Types.Currency
( Currency
)
where

import           OrderBook.Graph.Internal.Prelude

import qualified Data.Text                                  as T
import           Data.String                                (IsString)
import qualified Data.Aeson                                 as Json


-- | Currency code, e.g. "EUR", "BTC", "USD", "ETH"
newtype Currency = Currency T.Text
    deriving (Eq, Ord, Read, IsString, Semigroup, Monoid, Hashable, Generic, PrettyVal)

instance Show Currency where
    show (Currency txt) = "\"" <> toS txt <> "\""
instance NFData Currency
instance StringConv String Currency where
    strConv _ = fromString
instance StringConv Currency String where
    strConv l (Currency txt) = strConv l txt
instance StringConv Currency T.Text where
    strConv _ (Currency txt) = txt
instance StringConv T.Text Currency where
    strConv _ = Currency
instance Json.FromJSON Currency
