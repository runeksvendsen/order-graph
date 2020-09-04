{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module OrderBook.Graph
( withBidsAsksOrder
, readOrdersFile
, buildBuyGraph
, GraphInfo(..)
, IBuyGraph
, matchOrders
  -- * Re-exports
, module Export
)
where

import Prelude hiding (log)
import OrderBook.Graph.Internal.Prelude hiding (log)

import OrderBook.Graph.Build (CompactOrderList, Tagged)
import OrderBook.Graph.Exchange (invertSomeSellOrder)
import qualified OrderBook.Graph.Types.Book as Book
import qualified Data.Graph.Digraph as DG

import Data.Text (Text)
import Data.List (nub)
import qualified Data.Aeson as Json

-- Exports
import OrderBook.Graph.Types as Export
import OrderBook.Graph.Build as Export (build, buildFromOrders, SellOrderGraph)
import OrderBook.Graph.Match as Export (unlimited, BuyOrder, match, arbitrages)
import OrderBook.Graph.Run as Export (runArb, runMatch)


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
    -> Rational
    -> [Char]
    -> IO [OrderBook numType]
readOrdersFile log maxSlippage filePath = do
    log $ "Reading order books from " ++ show filePath ++ "..."
    books <- decodeFileOrFail filePath
    -- Log venues
    log ("Venues:") >> logVenues (nub $ map Book.bookVenue books)
    let orders = concatMap Book.fromOrderBook books
    log $ "Order book count: " ++ show (length books)
    log $ "Order count: " ++ show (length orders)
    -- TODO: print warning in case of input orderbook depth < 'maxSlippage'
    return $ map (Book.trimSlippageOB maxSlippage) books
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail :: (Json.FromJSON numType, Ord numType) => FilePath -> IO [OrderBook numType]
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file
    logVenues :: [Text] -> IO ()
    logVenues venues = forM_ venues $ \venue -> log ("\t" ++ toS venue)

buildGraph
    :: (Real numType)
    => (forall m. Monad m => String -> m ())
    -> [OrderBook numType]                         -- ^ Sell orders
    -> ST s (GraphInfo numType, SellOrderGraph s "arb")
buildGraph log sellOrders = do
    log "Building graph..."
    graph <- build sellOrders
    currencies <- DG.vertexLabels graph
    edgeCount <- DG.edgeCount graph
    let gi = GraphInfo { giVertices = currencies, giEdgeCount = edgeCount }
    log $ "Vertex count: " ++ show (length currencies)
    log $ "Edge count:   " ++ show edgeCount
    return (gi, graph)

-- NB: Phantom 'numType' is number type of input order book
data GraphInfo numType = GraphInfo
    { giVertices    :: [Currency]
    , giEdgeCount   :: Word
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
                (buyGraph, arbs) <- arbitrages src
                return (buyGraph, arbs : arbsAccum)
        (buyGraph, arbs) <- foldM findArbs (error "Empty graph", []) (giVertices gi)
        log $ unlines ["Arbitrages:", pp $ concat arbs]
        return buyGraph

type IBuyGraph = (DG.IDigraph Currency (Tagged "buy" CompactOrderList))

buildBuyGraph
    :: Real numType
    => (forall m. Monad m => String -> m ())
    -> [OrderBook numType]
    -> ST s (GraphInfo numType, IBuyGraph)
buildBuyGraph log sellOrders = do
    (gi, mGraph) <- buildGraph log sellOrders
    buyGraph <- DG.freeze =<< findArbitrages log gi mGraph
    return (gi, buyGraph)

matchOrders
    :: (KnownSymbol src, KnownSymbol dst)
    => (forall m. Monad m => String -> m ())
    -> BuyOrder dst src     -- ^ Buy cryptocurrency for national currency
    -> BuyOrder src dst     -- ^ Sell cryptocurrency for national currency
    -> IBuyGraph
    -> ST s ([SomeSellOrder], [SomeSellOrder])
matchOrders log buyOrder sellOrder buyGraph = do
    buyGraph' <- DG.thaw buyGraph
    runMatch buyGraph' $ do
        log "Matching sell order..."
        bids <- map invertSomeSellOrder <$> match sellOrder
        log "Matching buy order..."
        asks <- match buyOrder
        return (bids, asks)
