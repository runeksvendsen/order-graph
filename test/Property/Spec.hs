module Property.Spec where

import qualified OrderBook.Graph            as Lib
import Property.Orphans                     ()
import qualified Control.Category           as Cat
import qualified Data.Vector                as Vec

import Test.Hspec
import qualified Money
import qualified Test.Hspec.SmallCheck      as SC
import qualified Test.SmallCheck.Series     as SS
import Test.HUnit.Lang


-- | quantity of input orders equals quantity of output (matched) orders
quantityConservation
    :: [Lib.SomeSellOrder]
    -> Expectation
quantityConservation orders = undefined
