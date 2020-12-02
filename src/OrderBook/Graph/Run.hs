module OrderBook.Graph.Run
( runArb
, runMatch
)
where

import           OrderBook.Graph.Internal.Prelude
import qualified OrderBook.Graph.Build                      as B
import qualified OrderBook.Graph.Query                      as Q
import qualified Data.Graph.BellmanFord                     as BF
import qualified Data.Graph.Digraph                         as DG


runArb
    :: B.SellOrderGraph s "arb"
    -> Q.ArbGraphM s a
    -> ST s a
runArb graph =
    BF.runBF graph sumWeight 0

runMatch
    :: B.SellOrderGraph s "buy"
    -> Q.BuyGraphM s a
    -> ST s a
runMatch graph =
    BF.runBF graph sumWeight 0

sumWeight
    :: DG.HasWeight e Double
    => Double
    -> e
    -> Double
sumWeight weight' edge = weight' + DG.weight edge
