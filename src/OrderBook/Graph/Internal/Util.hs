{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module OrderBook.Graph.Internal.Util
( fromOB
, toSellBuyOrders
, merge
, trimSlippage
, assertAscendingPriceSorted
, lookupVertex
, lookupEdge
, union
, subgraph
, combine
, compress
) where

import           OrderBook.Graph.Internal.Prelude
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import qualified OrderBook.Types                            as OB
import qualified Money
import qualified Data.Text                                  as T
import qualified Data.Vector                                as Vec
import qualified Data.List.NonEmpty                         as NE
import           Data.String                                (fromString)
import           GHC.TypeLits                               (KnownSymbol, symbolVal)
import           Data.Proxy                                 (Proxy(..))
import qualified Data.Graph.Types                           as G
import qualified Data.Graph.Immutable                       as GI
import qualified Data.Graph.Mutable                         as GM


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

-- | Remove orders whose price is some specified percentage
--    further away than the first order's price.
--   E.g. "trimSlippage (10%1) sellOrders"
--    will remove the orders in "sellOrders" whose price is more/less than 10%
--    of the first order in "sellOrders"
trimSlippage
    :: Rational
    -- ^ Slippage in percent. E.g. 50%1 = 50%
    -> [SomeSellOrder]
    -- ^ List of orders sorted by price
    -> [SomeSellOrder]
trimSlippage _ [] = []
trimSlippage percentDifference (firstOrder : remainingOrders) =
    let startPrice = soPrice firstOrder
        filterByPricePercentage order =
            abs ((soPrice order - startPrice) / startPrice) <= (percentDifference / 100)
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

lookupVertex :: (Show v, Eq v) => G.Graph g e v -> v -> G.Vertex g
lookupVertex g v = fromMaybe (error $ "No such vertex: " ++ show v) (GI.lookupVertex v g)

lookupEdgeM :: (Show v, Eq v) => G.Graph g e v -> v -> v -> Maybe e
lookupEdgeM g from to =
    GI.lookupEdge (lookupVertex g from) (lookupVertex g to) g

lookupEdge :: (Show v, Eq v) => G.Graph g e v -> v -> v -> e
lookupEdge g from to = fromMaybe (error $ "No such edge: " ++ show (from, to)) $
    lookupEdgeM g from to

-- |
subgraph
    :: (PrimMonad m, Eq v, Hashable v, Show v)
    => G.MGraph (PrimState m) g1 e v     -- ^ Empty graph
    -> G.Graph g2 e v
    -> [(v, v)]                         -- ^ [(from, to)]
    -> m (G.Graph g1 e v)
subgraph mGraph g pathL = do
    forM_ pathL $ \(from, to) -> do
        fromV <- GM.insertVertex mGraph from
        toV <- GM.insertVertex mGraph to
        GM.insertEdge mGraph fromV toV (lookupEdge g from to)
    GI.freeze mGraph

-- |
union
    :: (PrimMonad m, Eq v, Hashable v, Show v)
    => G.MGraph (PrimState m) g1 e v     -- ^ Empty graph
    -> G.Graph g2 e v                    -- ^ Primary    (edges take precedence)
    -> G.Graph g3 e v                    -- ^ Secondary
    -> m (G.Graph g1 e v)
union mGraph ga gb = do
    Vec.forM_ allVertices $ \from -> do
        Vec.forM_ allVertices $ \to -> do
            fromV <- GM.insertVertex mGraph from
            toV <- GM.insertVertex mGraph to
            insertEdgeIfPresent mGraph gb from to fromV toV
            insertEdgeIfPresent mGraph ga from to fromV toV
    GI.freeze mGraph
  where
    insertEdgeIfPresent mGraph' g from to fromV toV =
        case GI.lookupEdge (lookupVertex g from) (lookupVertex g to) g of
            Nothing -> return ()
            Just e  -> GM.insertEdge mGraph' fromV toV e
    getVertices = GI.verticesToVector . GI.vertices
    allVertices = getVertices ga <> getVertices gb
