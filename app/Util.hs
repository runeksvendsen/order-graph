{-# LANGUAGE ScopedTypeVariables #-}
module Util
(getBookVenue)
where

import           OrderBook.Graph.Internal.Prelude
import           CryptoVenues.Types.ABook                   (ABook(ABook))
import qualified OrderBook.Types                            as OB
import qualified Data.Text                                  as T


-- | Return the venue of an ABook
getBookVenue :: ABook -> T.Text
getBookVenue (ABook ob) =
    bookVenue ob
  where
    bookVenue
        :: forall venue base quote. KnownSymbol venue
        => OB.OrderBook venue base quote
        -> T.Text
    bookVenue _ =
        toS $ symbolVal (Proxy :: Proxy venue)