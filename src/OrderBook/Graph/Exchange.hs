{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OrderBook.Graph.Exchange
( Qty
, Price
, Order
, maxQty
, minusQty
-- , fromSomeSellOrders
)
where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types                  (SomeSellOrder'(..), SomeSellOrder)
import qualified Control.Category                       as Cat
import           Data.Thrist
-- TMP
import Data.Functor.Identity


-- ^ Some quantity of "thing"
newtype Qty' numType (thing :: Symbol) = Qty' numType
    deriving (Eq, Show, Ord, Num)
type Qty = Qty' Double

-- ^ A price for exchanging some quantity of "src" for "dst"
newtype Price' numType (src :: Symbol) (dst :: Symbol) = Price' numType
    deriving (Eq, Show, Ord, Num)
type Price = Price' Double

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

-- ^ Exchange a quantity of something at a specified price
invert
    :: Fractional numType
    => Price' numType src dst
    -> Price' numType dst src
invert (Price' p) =
    Price' (recip p)

-- ^ A wish to exchange a given quantity of "src" for "dst"
--    at the given price
data Order' numType (src :: Symbol) (dst :: Symbol) = Order'
    (Qty' numType src)
    (Price' numType src dst)
        deriving (Eq, Show, Ord)
type Order = Order' Double

instance (Ord numType, Fractional numType) => Cat.Category (Order' numType) where
    id = Order' (Qty' $ 1/0) Cat.id
    Order' q1 p1 . Order' q2 p2 =
        let newQtyB = min q1 (exchange q2 p2)
            bToA = invert p2
        in Order' (exchange newQtyB bToA) (p1 Cat.. p2)


    


-- ^ Find the maximum quantity that can be moved from "A" to "Z"
--    through a list of 'Order's of the following form:
--
--   [Order A B, Order B C, Order C D, ..., Order X Y, Order Y Z]
maxQty
    :: Thrist Order src dst
    -- ^ List of orders of the specified form
    -> Order src dst
    -- ^ Order with a quantity equal to the maximum that can be moved through the
    --    given list of orders.
maxQty =
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
subtractExchange qty (Order' oQty oPrice) = 
    ( exchange qty oPrice
    , Order' (oQty-qty) oPrice
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

withSomeSellOrders
    :: SomeSellOrder
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => Order src dst -> r)
    -> r












{-

fromSomeSellOrders
    :: forall src dst. 
       (KnownSymbol src, KnownSymbol dst)
    => NonEmpty SomeSellOrder 
    -> Maybe (Thrist Order src dst)
fromSomeSellOrders (firstSSO :| restSSO) = do
    firstOrder <- fromSomeSellOrder firstSSO
    fromSomeSellOrdersR (firstOrder `Cons` Nil) restSSO
  where
    fromSomeSellOrdersR 
        :: (KnownSymbol b) 
        => Thrist Order b c 
        -> [SomeSellOrder] 
        -> Maybe (Thrist Order a c)
    fromSomeSellOrdersR t [] = return t
    fromSomeSellOrdersR t (x:xs) = do
        xOrder <- fromSomeSellOrder x
        fromSomeSellOrdersR (xOrder `Cons` t) xs

fromSomeSellOrder 
    :: forall src dst. 
       (KnownSymbol src, KnownSymbol dst)
    => SomeSellOrder 
    -> Maybe (Order src dst)
fromSomeSellOrder sso =
    case someSymbolVal (toS $ soBase sso) of
        SomeSymbol base ->
            case sameSymbol (Proxy :: Proxy dst) base of
                Nothing -> Nothing
                Just _ ->  
                    case someSymbolVal (toS $ soQuote sso) of
                        SomeSymbol quote ->
                            case sameSymbol (Proxy :: Proxy src) quote of
                                Nothing -> Nothing
                                Just _ -> Just $ Order' (Qty' $ soQty sso) (Price' $ soPrice sso)

-}