{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph.Exchange
( -- * Types
  Qty, rawQty
, Price, rawPrice
, Order, Order', oQty, oPrice
  -- * Utility functions
, maxOrder
, minusQty
, withSomeSellOrders
, toSomeSellOrder
, asList
, invertSomeSellOrder
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types                  (SomeSellOrder'(..), SomeSellOrder)
import           OrderBook.Graph.Query                  (ShortestPath(..))
import qualified OrderBook.Graph.Build                  as B
import qualified Control.Category                       as Cat
import           Data.Thrist
import qualified Data.Text                              as T
import qualified Data.List.NonEmpty                     as NE


-- ^ Some quantity of "thing"
newtype Qty' numType (thing :: Symbol) = Qty' numType
    deriving (Eq, Ord, Num)
type Qty = Qty' Rational

rawQty :: Qty' numType thing -> numType
rawQty (Qty' qty) = qty

instance (KnownSymbol thing, Real numType) => Show (Qty' numType thing) where
    show (Qty' qty) =
        printf "%f %s" (realToFrac qty :: Double) (symbolVal (Proxy :: Proxy thing))

-- ^ A price for exchanging some quantity of "src" for "dst"
newtype Price' numType (src :: Symbol) (dst :: Symbol) = Price' numType
    deriving (Eq, Show, Ord, Num)
type Price = Price' Rational

rawPrice :: Price' numType src dst -> numType
rawPrice (Price' price) = price

instance Num numType => Cat.Category (Price' numType) where
    id = Price' 1
    Price' p1 . Price' p2 = Price' (p1 * p2)

-- ^ Exchange a quantity of something at a specified price
exchange
    :: Num numType
    => Qty' numType src         -- ^ What you sell
    -> Price' numType src dst   -- ^ The price at which you're willing to sell
    -> Qty' numType dst         -- ^ What you earn
exchange (Qty' srcQty) (Price' price) =
    Qty' (srcQty * price)

-- ^ Invert a 'Price'
invert
    :: Fractional numType
    => Price' numType src dst
    -> Price' numType dst src
invert (Price' p) =
    Price' (recip p)

-- ^ A wish to exchange a given quantity of "src" for "dst"
--    at a specific price
data Order' numType (src :: Symbol) (dst :: Symbol) = Order'
    (Qty' numType src)
    (Price' numType src dst)
        deriving (Eq, Show, Ord)
type Order = Order' Rational

oQty :: Order' numType src dst -> Qty' numType src
oQty (Order' qty _) = qty

oPrice :: Order' numType src dst -> Price' numType src dst
oPrice (Order' _ price) = price

invertOrder
    :: Order src dst
    -> Order dst src
invertOrder (Order' qty price) =
    Order' (exchange qty price) (invert price)

instance Cat.Category Order where
    id = Order' (Qty' largeRational) Cat.id -- TODO: HACK
    Order' q1 p1 . Order' q2 p2 =
        let newQtyB = min q1 (exchange q2 p2)
            bToA = invert p2
        in Order' (exchange newQtyB bToA) (p1 Cat.. p2)

-- ^ Find the maximum quantity that can be moved from "A" to "Z"
--    through a list of 'Order's of the following form:
--
--   [Order A B, Order B C, Order C D, ..., Order X Y, Order Y Z]
maxOrder
    :: Thrist Order src dst
    -- ^ List of orders of the specified form
    -> Order src dst
    -- ^ Maximum quantity that can be moved through the given list of orders.
maxOrder =
    foldrThrist (flip (Cat..)) Cat.id

-- ^ Subtract the given quantity from the quantities of all the orders in the list
minusQty
    :: Thrist Order src dst
    -> Qty src
    -> Thrist Order src dst
minusQty Nil        _   = Nil
minusQty (Cons h t) qty =
    let (newQty, newOrder) = subtractExchange qty h
    in newOrder `Cons` minusQty t newQty

-- ^ Given a quantity and an order (with quantity "src" == order "src")
--    return the quantity exchanged to "dst" (using the order's price)
--    and the order with its quantity subtracted by the given quantity.
subtractExchange
    :: Num numType
    => Qty' numType src
    -> Order' numType src dst
    -> (Qty' numType dst, Order' numType src dst)
subtractExchange qty (Order' oQty' oPrice') =
    ( exchange qty oPrice'
    , Order' (oQty'-qty) oPrice'
    )

withSomeSellOrder
    :: SomeSellOrder
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Order src dst -> r)
    -> r
withSomeSellOrder sso f =
    case someSymbolVal (toS $ soBase sso) of
        SomeSymbol (Proxy :: Proxy src) ->
            case someSymbolVal (toS $ soQuote sso) of
                SomeSymbol (Proxy :: Proxy dst) ->
                    let order = Order' (Qty' $ soQty sso) (Price' $ soPrice sso)
                    in f (order :: Order src dst)

-- |
withSomeSellOrders
    :: ShortestPath
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => NonEmpty B.SortedOrders -> Thrist Order src dst -> r)
    -> r
withSomeSellOrders (ShortestPath sortedOrders) f' =
    let revSortedOrders = NE.reverse sortedOrders in
    go (fmap B.first revSortedOrders) (f' revSortedOrders)
  where
    -- Join a list of 'SomeSellOrder' to produce a 'Thrist' parameterized over
    --    the source and destination currency.
    --   The sequence of input sell orders must be of the following form:
    --      [ Order "BTC" "USD"
    --      , Order "USD" "EUR"
    --      , Order "EUR" "ETH"
    --      , Order "ETH" "LOL"
    --      ]
    --    ie. for any two adjacent sell orders the left order's "base"/"src" must be
    --    equal to the right order's "quote"/"dst".
    go :: NonEmpty SomeSellOrder
       -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Thrist Order src dst -> r)
       -> r
    go sellOrders' f =
        case uncons sellOrders' of
            (sso1, Nothing) ->
                withSomeSellOrder sso1 $ \order -> f (order `Cons` Nil)
            (sso1, Just ssoTail) ->
                withSomeSellOrder sso1 $ \(order :: Order src dst1) ->
                    go ssoTail $ \(thrist :: Thrist Order src2 dst) ->
                        case sameSymbol (Proxy :: Proxy dst1) (Proxy :: Proxy src2) of
                            Nothing -> error $ "Order path hole: " ++ pp (sso1, ssoTail)
                            Just Refl  -> f (order `Cons` thrist)

asList
    :: (forall src dst. Order src dst -> r)
    -> Thrist Order a b
    -> [r]
asList _ Nil        = []
asList f (Cons h t) = f h : asList f t

instance (KnownSymbol src, KnownSymbol dst) => Show (Thrist Order src dst) where
    show = show . asList
        (\o -> ( realToFrac . rawQty . oQty $ o :: Double
               , realToFrac . rawPrice . oPrice $ o :: Double
               )
        )

toSomeSellOrder
    :: forall src dst.
       (KnownSymbol src, KnownSymbol dst)
    => Order src dst    -- ^ Order
    -> T.Text           -- ^ Venue
    -> SomeSellOrder
toSomeSellOrder order venue = SomeSellOrder'
    { soPrice = rawPrice $ oPrice order
    , soQty   = rawQty $ oQty order
    , soBase  = fromString $ symbolVal (Proxy :: Proxy src)
    , soQuote = fromString $ symbolVal (Proxy :: Proxy dst)
    , soVenue = venue
    }

invertSomeSellOrder
    :: SomeSellOrder
    -> SomeSellOrder
invertSomeSellOrder sso =
    withSomeSellOrder sso $ \order ->
        toSomeSellOrder (invertOrder order) (soVenue sso)
