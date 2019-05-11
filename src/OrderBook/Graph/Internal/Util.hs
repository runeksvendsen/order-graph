{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module OrderBook.Graph.Internal.Util
( fromOB
, fromABook
, withABook
, baseQuote
, toSellBuyOrders
, merge
, trimSlippage
, trimSlippageOB
, assertAscendingPriceSorted
, combine
, compress
) where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import qualified OrderBook.Types                            as OB
import           CryptoVenues.Types.ABook                   (ABook(ABook))

import qualified Money
import qualified Data.Text                                  as T
import qualified Data.Vector                                as Vec
import qualified Data.List.NonEmpty                         as NE
import           Data.String                                (fromString)
import           GHC.TypeLits                               (KnownSymbol, symbolVal)
import           Data.Proxy                                 (Proxy(..))


-- | Convert all orders in an orderbook (consisting of both sell orders and buy orders)
--    into a list of sell orders
fromOB
    :: forall venue base quote.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => OB.OrderBook venue base quote
    -> [SomeSellOrder]
fromOB ob =
    sellOrders ++ buyOrders
  where
    (sellOrders, buyOrders) = toSellBuyOrders ob

withABook
    :: forall r.
    ( forall venue base quote.
      (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
      => OB.OrderBook venue base quote
      -> r
    )
    -> ABook
    -> r
withABook f (ABook ob) = f ob

fromABook :: ABook -> [SomeSellOrder]
fromABook = withABook fromOB

baseQuote :: ABook -> (T.Text, T.Text)
baseQuote (ABook ob) =
    fromOb ob
  where
    fromOb :: forall venue base quote.
              (KnownSymbol base, KnownSymbol quote)
           => OB.OrderBook venue base quote
           -> (T.Text, T.Text)
    fromOb _ = ( toS $ symbolVal (Proxy :: Proxy base)
               , toS $ symbolVal (Proxy :: Proxy quote)
               )

-- | Convert all orders in an orderbook (consisting of both sell orders and buy orders)
--    into a pair of sell orders, where the first item is sell orders and
toSellBuyOrders
    :: forall venue base quote.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => OB.OrderBook venue base quote
    -> ([SomeSellOrder], [SomeSellOrder])   -- ^ (Sell orders, buy orders)
toSellBuyOrders OB.OrderBook{..} =
    ( map (fromSellOrder venue) (Vec.toList $ OB.sellSide obAsks)
    , map (fromSellOrder venue) (map OB.invert . Vec.toList $ OB.buySide obBids)
    )
  where
    venue = fromString $ symbolVal (Proxy :: Proxy venue)

fromSellOrder
    :: forall base quote.
       (KnownSymbol base, KnownSymbol quote)
    => T.Text                   -- ^ Venue
    -> OB.Order base quote      -- ^ Sell order
    -> SomeSellOrder
fromSellOrder venue OB.Order{..} = SomeSellOrder'
    { soPrice = fromRational $ Money.exchangeRateToRational oPrice
    , soQty   = fromRational $ toRational oQuantity
    , soBase  = fromString $ symbolVal (Proxy :: Proxy base)
    , soQuote = fromString $ symbolVal (Proxy :: Proxy quote)
    , soVenue = venue
    }

-- | merge adjacent orders with same price (ignoring venue)
merge
    :: [SomeSellOrder]
    -- ^ List of orders sorted by price ("soBase" and "soQuote" must be the same for all orders)
    -> [SomeSellOrder]
merge =
    combine tryMergeOrders
  where
    mergeOrders order1 order2 = order1 { soQty = soQty order1 + soQty order2 }
    tryMergeOrders order1 order2 =
        if soPrice order1 == soPrice order2
            then Just (mergeOrders order1 order2)
            else Nothing

-- | Combine adjacent list items. Only defined for lists of length >= 2.
--
--   Invariants:
--      "combine (const $ const Nothing) = id"
--      "combine (const $ const $ Just value) _ = [value]"
combine
    :: (a -> a -> Maybe a)
    -- ^ If the two adjacent list items can be combined,
    --  return 'Just' of an item that is the combination of the two items,
    --  otherwise 'Nothing'.
    -> [a]
    -> [a]
combine f =
    reverse . foldl (\accum item -> combine' accum item) []
  where
    combine' [] item = [item]
    combine' accumList@(newestItem : remainingItems) item =
        case f newestItem item of
            Just combinedA -> combinedA : remainingItems
            Nothing        -> item : accumList

trimSlippage
    :: Rational
    -- ^ Slippage in percent. E.g. 50%1 = 50%
    -> [SomeSellOrder]
    -- ^ List of orders sorted by price
    -> [SomeSellOrder]
trimSlippage = trimSlippageGeneric soPrice

-- | Remove orders whose price is some specified percentage
--    further away than the first order's price.
--   E.g. "trimSlippage 10 sellOrders"
--    will remove the orders in "sellOrders" whose price is more/less than 10%
--    of the first order in "sellOrders"
trimSlippageGeneric
    :: (Fractional numType, Ord numType)
    => (order -> numType)
    -> numType
    -- ^ Slippage in percent. E.g. 50%1 = 50%
    -> [order]
    -- ^ List of trimmed orders sorted by price
    -> [order]
trimSlippageGeneric _ _ [] = []
trimSlippageGeneric oPrice percentDifference (firstOrder : remainingOrders) =
    let startPrice = oPrice firstOrder
        filterByPricePercentage order =
            abs ((oPrice order - startPrice) / startPrice) <= (percentDifference / 100)
    in firstOrder : filter filterByPricePercentage remainingOrders

trimSlippageOB maxSlippage (ABook ob) = ABook $
    trimSlippageOB' maxSlippage ob

-- ^ Same as "trimSlippage" but do it for an order book
trimSlippageOB'
    :: Rational
    -- ^ Slippage in percent. E.g. 50%1 = 50%
    -> OB.OrderBook venue base quote
    -> OB.OrderBook venue base quote
trimSlippageOB' maxSlippage ob =
    let buySide = OB.buyOrders ob
        sellSide = OB.sellOrders ob
        rationalPrice = Money.exchangeRateToRational . OB.oPrice
        trimObSide =
            Vec.fromList . trimSlippageGeneric rationalPrice maxSlippage . Vec.toList
    in OB.OrderBook
            (OB.BuySide  $ trimObSide buySide)
            (OB.SellSide $ trimObSide sellSide)

-- | Merge a large number of orders into a smaller number of orders
--    by merging the volume of adjacent orders into a single order, so
--    that a maximum number of orders remain. Not lossless.
compress
    :: Word
    -- ^ Maximum number of orders after compressing
    -> [SomeSellOrder]
    -- ^ List of orders sorted by price
    -> [SomeSellOrder]
compress _ [] = []
compress maxCount orderList@(firstOrder : remainingOrders) =
    reverse . NE.toList . fst $ foldl compress' (firstOrder :| [], 0) remainingOrders
  where
    skipCount = fromIntegral (length orderList) `quot` maxCount
    plusQtyOf baseOrder qtyOrder = baseOrder { soQty = soQty baseOrder + soQty qtyOrder}
    compress' (accum@(headOrder :| tailOrders), count) order =
        if count < skipCount
            then (headOrder `plusQtyOf` order :| tailOrders, count+1)
            else (order `cons` accum, 0)

assertAscendingPriceSorted
    :: [SomeSellOrder]
    -> IO ()
assertAscendingPriceSorted [] = return ()
assertAscendingPriceSorted (firstOrder : remainingOrders) =
    void $ foldM adjacentOrdersSorted firstOrder remainingOrders
  where
    adjacentOrdersSorted :: SomeSellOrder -> SomeSellOrder -> IO SomeSellOrder
    adjacentOrdersSorted prevOrder nextOrder =
        if soPrice prevOrder <= soPrice nextOrder
            then return nextOrder   -- Everything is ok
            else do
                putStrLn $ unlines ["Orders not sorted:", pp prevOrder, pp nextOrder]
                return nextOrder
