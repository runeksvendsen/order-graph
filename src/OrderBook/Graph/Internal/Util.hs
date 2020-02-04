{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module OrderBook.Graph.Internal.Util
( merge
, trimSlippageGeneric
, assertAscendingPriceSorted
, combine
, compress
) where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types.SomeSellOrder        (SomeSellOrder, SomeSellOrder'(..))

import qualified Data.List.NonEmpty                         as NE


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
