module Main where

import qualified Unit.Spec                          as Unit
import           OrderBook.Graph.Internal.Prelude
import qualified OrderBook.Graph                    as Lib
import qualified Data.Graph.Immutable               as GI

import           Test.HUnit
import qualified Data.List.NonEmpty                 as NE
import           Test.Hspec.Expectations.Pretty


main :: IO Counts
main = runTestTT Unit.tests
