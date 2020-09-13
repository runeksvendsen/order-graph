module Main where

import qualified Unit.Spec                          as Unit
import qualified Unit.Integration
import qualified Property.Combine                   as Combine
import qualified Property.Build                     as Build
import qualified Property.Match                     as Match

import           OrderBook.Graph.Internal.Prelude

import           Test.HUnit
import qualified Test.Hspec.Runner                  as Run
import           Test.Hspec                         (parallel)


main :: IO ()
main = do
    void $ runTestTT $ TestList [Unit.tests, Unit.Integration.tests]
    runHspec $ parallel $ do
        Combine.spec
        Build.spec
        Match.spec
  where
    runHspec = Run.hspecWith Run.defaultConfig { Run.configSmallCheckDepth = scDepth }
    scDepth = 4
