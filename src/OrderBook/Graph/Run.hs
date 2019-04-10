module OrderBook.Graph.Run
( runArb
, runMatch
)
where

import           OrderBook.Graph.Internal.Prelude
import qualified OrderBook.Graph.Build                      as B
import qualified OrderBook.Graph.Query                      as Q
import qualified Data.Graph.BellmanFord                     as BF
import qualified Control.Monad.ST                           as ST


runArb
    :: B.SellOrderGraph s g "arb"
    -> Q.ArbGraphM s g a
    -> ST s a
runArb graph =
    BF.runBF graph sumWeight
  where
    sumWeight
        :: Double
        -> B.Tagged "arb" B.SortedOrders
        -> Double
    sumWeight weight' edge = weight' + BF.weight edge

runMatch
    :: B.SellOrderGraph s g "buy"
    -> Q.BuyGraphM s g a
    -> ST s a
runMatch graph =
    BF.runBF graph multiplyWeight
  where
    multiplyWeight
        :: Double
        -> B.Tagged "buy" B.SortedOrders
        -> Double
    multiplyWeight weight' edge = weight' * BF.weight edge
