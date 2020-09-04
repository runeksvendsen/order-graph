module OrderBook.Graph.Run
( runArb
, runMatch
)
where

import           OrderBook.Graph.Internal.Prelude
import qualified OrderBook.Graph.Build                      as B
import qualified OrderBook.Graph.Query                      as Q
import qualified Data.Graph.BellmanFord                     as BF


runArb
    :: B.SellOrderGraph s g "arb"
    -> Q.ArbGraphM s g a
    -> ST s a
runArb graph =
    BF.runBF graph sumWeight

runMatch
    :: B.SellOrderGraph s g "buy"
    -> Q.BuyGraphM s g a
    -> ST s a
runMatch graph =
    BF.runBF graph sumWeight

sumWeight
    :: BF.WeightedEdge e v Double
    => Double
    -> e
    -> Double
sumWeight weight' edge = weight' + BF.weight edge
