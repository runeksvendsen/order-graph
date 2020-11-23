{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module OrderBook.Graph.Internal.Prelude
( module Export
, module Prim
, module Monad
, module MoreStuff
, justOrFail
, pp
, pprint
, largeRational
, largeDouble
, uppercase
, T.Text
, lift
)

where

import Prelude                              as Export
import Protolude.Conv                       as Export
import Protolude.Safe                       as Export
import GHC.TypeLits                         as Export
import Data.Proxy                           as Export
import Data.Maybe                           as Export
import Control.Monad.Primitive              as Prim         (PrimMonad, PrimState)
import Control.Monad                        as Monad
import Data.List.NonEmpty                   as MoreStuff    (NonEmpty(..), cons, uncons, nonEmpty)
import Data.String                          as MoreStuff    (fromString)
import GHC.Generics                         as MoreStuff    (Generic)
import Control.DeepSeq                      as MoreStuff    (NFData)
import Control.Monad.Fix                    as MoreStuff    (mfix)
import Text.Show.Pretty                     as MoreStuff    (PrettyVal(..), valToStr)
import Data.Type.Equality                   as MoreStuff
import Text.Printf                          as MoreStuff
import Data.Ratio                           as MoreStuff
import Data.Hashable                        as MoreStuff    (Hashable)
import Control.Monad.ST                     as MoreStuff
import Control.Exception                    as MoreStuff    (assert)
import Debug.Trace                          as MoreStuff
import Data.Int                             (Int64)
import Data.Char                            (toUpper)
import qualified Data.Text                  as T
import Protolude                            (lift)


pp :: PrettyVal a => a -> String
pp = valToStr . prettyVal

pprint :: PrettyVal a => a -> IO ()
pprint = putStrLn . valToStr . prettyVal

justOrFail :: PrettyVal a => ([Char], a) -> Maybe b -> b
justOrFail (textMsg, a) =
    fromMaybe . error $ unlines
        [ textMsg ++ ":"
        , pp a
        ]

instance PrettyVal a => PrettyVal (NonEmpty a) where
    prettyVal (first :| lst) = prettyVal (first : lst)

largeRational :: Rational
largeRational = fromIntegral (maxBound :: Int64) % 1

largeDouble :: Double
largeDouble = realToFrac largeRational

uppercase :: String -> String
uppercase = map toUpper
