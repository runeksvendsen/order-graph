module OrderBook.Graph.Match.Types
( BuyOrder
, BuyOrder'(..)
, unlimited
, MatchResult
, MatchResult'(..)
, empty
, addOrder
, orderFilled
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types.Path (Path')
import OrderBook.Graph.Types.SomeSellOrder (NumType)


-- |
newtype BuyOrder' numTyp (dst :: Symbol) (src :: Symbol) = BuyOrder'
    { boQuantity        :: Maybe numTyp -- TODO: ignored for now
    }

type BuyOrder = BuyOrder' NumType

-- | A buy order whose execution will continue until there
--    is no path from 'src' to 'dst'.
unlimited
    :: Fractional numTyp
    => BuyOrder' numTyp dst src
unlimited = BuyOrder'
    { boQuantity    = Nothing
    }

type MatchResult = MatchResult' NumType
newtype MatchResult' numTyp = MatchResult'
    { mrOrders      :: [Path' numTyp]
    } deriving (Eq, Show)

empty :: Num numTyp => MatchResult' numTyp
empty = MatchResult'
    { mrOrders      = []
    }

addOrder
    :: (Real numTyp, Show numTyp)
    => MatchResult' numTyp
    -> Path' numTyp
    -> MatchResult' numTyp
addOrder (MatchResult' orders) order = MatchResult' (order: orders)

-- | Stop order execution if this returns 'True'
--
--  TODO: check quantity
orderFilled
    :: (Fractional numTyp, Real numTyp, Show numTyp)
    => BuyOrder' numTyp base quote
    -> MatchResult' numTyp
    -> Bool
orderFilled _ (MatchResult' _) = False
