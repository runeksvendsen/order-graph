module Main where

import qualified Unit.Spec                          as Unit
import qualified Property.Spec                      as Property
import           OrderBook.Graph.Internal.Prelude
import qualified OrderBook.Graph                    as Lib
import qualified Data.Graph.Immutable               as GI

import           Test.HUnit
import qualified Data.List.NonEmpty                 as NE
import           Test.Hspec.Expectations.Pretty
import           Test.Hspec.Runner


scDepth = 7

main :: IO Counts
main = do
    hspecWith defaultConfig { configSmallCheckDepth = scDepth } Property.spec
    runTestTT Unit.tests
