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
import           OrderBook.Graph.Build                      (SomeSellOrder'(..))


-- |
data BuyOrder' numTyp (dst :: Symbol) (src :: Symbol) = BuyOrder'
    { boQuantity        :: Maybe numTyp
    , boMaxPrice        :: Maybe numTyp
      -- ^ (TODO: IGNORED FOR NOW) maximum price
    , boMaxSlippage     :: Maybe numTyp
      -- ^ maximum percentage difference between price of first and last matched order
    }

type BuyOrder = BuyOrder' Rational

-- | A buy order whose execution will continue until there
--    is no path from 'src' to 'dst'.
unlimited
    :: Fractional numTyp
    => BuyOrder' numTyp dst src
unlimited = BuyOrder'
    { boQuantity    = Nothing
    , boMaxPrice    = Nothing
    , boMaxSlippage = Nothing
    }

type MatchResult = MatchResult' Rational
data MatchResult' numTyp = MatchResult'
    { mrOrders      :: [SomeSellOrder' numTyp]
    , mrFirstOrder  :: Maybe (SomeSellOrder' numTyp)
    , mrQuantity    :: numTyp
    } deriving (Eq, Show)

empty :: Num numTyp => MatchResult' numTyp
empty = MatchResult'
    { mrOrders      = []
    , mrFirstOrder  = Nothing
    , mrQuantity    = 0
    }

addOrder
    :: (Real numTyp, Show numTyp)
    => MatchResult' numTyp
    -> SomeSellOrder' numTyp
    -> MatchResult' numTyp
addOrder (MatchResult' [] Nothing _) order =
    MatchResult' [order] (Just order) (soQty order)
addOrder (MatchResult' orders firstOrder@Just{} qty) order =
    MatchResult' (order : orders) firstOrder (qty + soQty order)
addOrder mr@(MatchResult' _ Nothing _) _ =
    error $ "invalid MatchResult' " ++ show mr

-- | Stop order execution if this returns 'True'
--
--  TODO: check maximum price
orderFilled
    :: (Fractional numTyp, Real numTyp, Show numTyp)
    => BuyOrder' numTyp base quote
    -> MatchResult' numTyp
    -> Bool
orderFilled _ (MatchResult' _ Nothing _) = False
orderFilled _ mr@(MatchResult' [] Just{} _) = error $ "invalid MatchResult' " ++ show mr
orderFilled (BuyOrder' qtyM _ slipM) (MatchResult' (latest:_) (Just first) mrQty) =
    qtyFilled || slippageReached
  where
    checkProp propM f = maybe False f propM
    qtyFilled = checkProp qtyM $ \qty -> mrQty >= qty
    slippageReached = checkProp slipM $ \maxSlippage ->
        let slippagePct = abs $ (soPrice latest - soPrice first) / soPrice first * 100
        in slippagePct > maxSlippage
