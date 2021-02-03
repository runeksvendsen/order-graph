{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph
( -- * Read input data; build graph; match orders
  withBidsAsksOrder
, readOrdersFile
, buildBuyGraph
, GraphInfo(..)
, IBuyGraph
, matchOrders
  -- * Process output of order matching
, LiquidityInfo(..)
, SideLiquidity(..)
, PriceRange(..)
, toLiquidityInfo
, toSideLiquidity
  -- * Re-exports
, module Export
)
where

import Prelude hiding (log)
import OrderBook.Graph.Internal.Prelude hiding (log)

import OrderBook.Graph.Build (CompactOrderList, Tagged)
import qualified OrderBook.Graph.Types.Book as Book
import qualified Data.Graph.Digraph as DG

import Data.Text (Text)
import Data.List (sortOn, sortBy, nub, groupBy)
import qualified Data.Aeson as Json
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T


-- Exports
import OrderBook.Graph.Types as Export
import OrderBook.Graph.Build as Export (build, buildFromOrders, SellOrderGraph)
import OrderBook.Graph.Match as Export (unlimited, BuyOrder, match, arbitrages)
import OrderBook.Graph.Run as Export (runArb, runMatch)
import OrderBook.Graph.Types.Path as Export
import OrderBook.Graph.Exchange as Export (invertSomeSellOrder)
import Data.Ord (comparing)


-- |
withBidsAsksOrder
    :: Currency -- ^ Numeraire
    -> Currency -- ^ Cryptocurrency
       -- | arg1: buy order, arg2: sell order
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => BuyOrder dst src
                                                           -> BuyOrder src dst
                                                           -> r
       )
    -> r
withBidsAsksOrder numeraire crypto f =
    case someSymbolVal (toS numeraire) of
        SomeSymbol (Proxy :: Proxy numeraire) ->
            case someSymbolVal (toS crypto) of
                SomeSymbol (Proxy :: Proxy crypto) ->
                    f (buyOrder :: BuyOrder crypto numeraire)
                      (buyOrder :: BuyOrder numeraire crypto)
  where
    buyOrder = unlimited

readOrdersFile
    :: (Json.FromJSON numType, Fractional numType, Real numType)
    => ([Char] -> IO ())
    -> [Char]
    -> IO [OrderBook numType]
readOrdersFile log filePath = do
    log $ "Reading order books from " ++ show filePath ++ "..."
    books <- decodeFileOrFail filePath
    -- Log venues
    log ("Venues:") >> logVenues (nub $ map Book.bookVenue books)
    let orders = concatMap Book.fromOrderBook books
    log $ "Order book count: " ++ show (length books)
    log $ "Order count: " ++ show (length orders)
    return books
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail :: (Json.FromJSON numType, Ord numType) => FilePath -> IO [OrderBook numType]
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file
    logVenues :: [Text] -> IO ()
    logVenues venues = forM_ venues $ \venue -> log ("\t" ++ toS venue)

buildGraph
    :: (Real numType, Fractional numType)
    => (forall m. Monad m => String -> m ())
    -> Rational
    -> [OrderBook numType]
    -> ST s (GraphInfo numType, SellOrderGraph s "arb")
buildGraph log maxSlippage books = do
    log "Building graph..."
    graph <- build trimmedBooks
    currencies <- DG.vertexLabels graph
    edgeCount <- DG.edgeCount graph
    let gi = GraphInfo { giVertices = currencies, giEdgeCount = edgeCount, giWarnings = catMaybes warningMs }
    log $ "Vertex count: " ++ show (length currencies)
    log $ "Edge count:   " ++ show edgeCount
    return (gi, graph)
  where
    (trimmedBooks, warningMs) = unzip $ map (Book.trimSlippageOB maxSlippage . sortOrders) books

-- NB: Phantom 'numType' is number type of input order book
data GraphInfo numType = GraphInfo
    { giVertices    :: [Currency]
    , giEdgeCount   :: Word
    , giWarnings    :: [Text]
    }

-- | Find and remove all arbitrages from the input graph.
findArbitrages
    :: (forall m. Monad m => String -> m ())
    -> GraphInfo numType
    -> SellOrderGraph s "arb"
    -> ST s (SellOrderGraph s "buy")
findArbitrages log gi graph = do
    runArb graph $ do
        log "Finding arbitrages..."
        let findArbs (_, arbsAccum) src = do
                log $ "\t" ++ toS src
                (buyGraph, arbs) <- arbitrages src
                return (buyGraph, arbs : arbsAccum)
        (buyGraph, arbs) <- foldM findArbs (error "Empty graph", []) (giVertices gi)
        log (arbLogStr $ concat arbs)
        return buyGraph
  where
    arbLogStr paths = toS $ T.unlines $ map (T.unlines . map ("\t" <>) . printGroup) $ groupOn (pStart . pathDescr) paths
    printGroup paths =
        T.unwords [toS (pStart . pathDescr $ head paths), toS . show @Double . realToFrac $ pathsQty paths]
        : map (("\t" <>) . showPath . head) (groupOn pathDescr paths)
    pathsQty :: [Path] -> NumType
    pathsQty = sum . map pQty
    groupOn f = groupBy (\a1 a2 -> f a1 == f a2) . sortOn f

type IBuyGraph = (DG.IDigraph Currency (Tagged "buy" CompactOrderList))

buildBuyGraph
    :: (Real numType, Fractional numType)
    => (forall m. Monad m => String -> m ())
    -> Rational
    -> [OrderBook numType]
    -> ST s (GraphInfo numType, IBuyGraph)
buildBuyGraph log maxSlippage sellOrders = do
    (gi, mGraph) <- buildGraph log maxSlippage sellOrders
    buyGraph <- DG.freeze =<< findArbitrages log gi mGraph
    return (gi, buyGraph)

-- |
matchOrders
    :: (forall m. Monad m => String -> m ()) -- ^ logger
    -> Currency -- ^ numeraire
    -> Currency -- ^ cryptocurrency
    -> IBuyGraph
    -> ST s ([SellPath], [BuyPath])
matchOrders log numeraire crypto buyGraph = withBidsAsksOrder numeraire crypto $ \buyOrder sellOrder -> do
    buyGraph' <- DG.thaw buyGraph
    runMatch buyGraph' $ do
        log "Matching sell order..."
        sellPath <- map toSellPath <$> match sellOrder
        log "Matching buy order..."
        buyPath <- map toBuyPath <$> match buyOrder
        return (sellPath, buyPath)


-- | Liquidity info in both buy and sell direction
data LiquidityInfo = LiquidityInfo
    { liBuyLiquidity    :: Maybe SideLiquidity
    , liSellLiquidity   :: Maybe SideLiquidity
    } deriving (Eq, Show)

-- | Liquidity info in a single direction (either buy or sell)
data SideLiquidity = SideLiquidity
    { liLiquidity    :: NumType             -- ^ Non-zero liquidity
    , liPriceRange   :: PriceRange NumType
    , liPaths        :: NonEmpty (NumType, PriceRange NumType, PathDescr)  -- ^ (quantity, price_range, path)
    } deriving (Eq, Show)

data PriceRange numType =
    PriceRange
        { lowestPrice :: numType
        , highestPrice :: numType
        } deriving (Eq, Show)

toLiquidityInfo
    :: ([SellPath], [BuyPath])
    -> Maybe LiquidityInfo
toLiquidityInfo (sellPath, buyPath) = do
    Just $ LiquidityInfo
        { liBuyLiquidity    = toSideLiquidity <$> NE.nonEmpty buyPath
        , liSellLiquidity   = toSideLiquidity <$> NE.nonEmpty sellPath
        }

toSideLiquidity
    :: forall path.
       HasPathQuantity path NumType
    => NE.NonEmpty path
    -> SideLiquidity
toSideLiquidity nonEmptyOrders =
    let paths = NE.fromList $ sortByQuantity $ map quoteSumVenue (groupByPath $ NE.toList nonEmptyOrders)
    in SideLiquidity
        { liLiquidity    = quoteSum nonEmptyOrders
        , liPriceRange   = firstLastPrice nonEmptyOrders
        , liPaths        = paths
        }
  where
    firstLastPrice lst =
        let priceSorted = NE.sortBy (comparing pPrice) lst
        in PriceRange (pPrice $ NE.head priceSorted) (pPrice $ NE.last priceSorted)
    quoteSumVenue paths =
        (quoteSum paths, priceRange paths, pathDescr $ NE.head paths)
    groupByPath = NE.groupBy (\a b -> pathDescr a == pathDescr b) . sortOn pathDescr
    sortByQuantity = sortBy (flip $ comparing $ \(quoteQty, _, _) -> quoteQty)
    quoteSum orderList = sum $ NE.map pQty orderList
    priceRange
        :: NE.NonEmpty path
        -> PriceRange NumType
    priceRange soList =
        let priceList = NE.map pPrice soList
        in PriceRange (minimum priceList) (maximum priceList)
