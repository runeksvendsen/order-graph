{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module OrderBook.Graph
( withBidsAsksOrder
, readOrdersFile
, buildGraph
, GraphInfo(..)
, matchOrders
  -- * Re-exports
, module Export
)
where

import Prelude hiding (log)
import OrderBook.Graph.Internal.Prelude hiding (log)
import OrderBook.Graph.Types as Export
import OrderBook.Graph.Build
import OrderBook.Graph.Exchange
import OrderBook.Graph.Match
import OrderBook.Graph.Run
import qualified OrderBook.Graph.Types.Book as Book
import qualified Data.Graph.Digraph as DG

import Data.List (nub)
import qualified Control.Monad.ST as ST
import qualified Data.Aeson as Json



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
    => [OrderBook numType]                         -- ^ Sell orders
    -> ST s (GraphInfo numType, SellOrderGraph s "arb")
buildGraph sellOrders = do
    graph <- build sellOrders
    currencies <- DG.vertexLabels graph
    edgeCount <- DG.edgeCount graph
    let gi = GraphInfo { giVertices = currencies, giEdgeCount = edgeCount }
    return (gi, graph)

-- NB: Phantom 'numType' is number type of input order book
data GraphInfo numType = GraphInfo
    { giVertices    :: [Currency]
    , giEdgeCount   :: Word
    }

matchOrders
    :: (KnownSymbol src, KnownSymbol dst, Real numType)
    => (forall m. Monad m => String -> m ())
    -> BuyOrder dst src     -- ^ Buy cryptocurrency for national currency
    -> BuyOrder src dst     -- ^ Sell cryptocurrency for national currency
    -> [OrderBook numType]      -- ^ Input orders
    -> IO ([SomeSellOrder], [SomeSellOrder])    -- ^ (bids, asks)
matchOrders log buyOrder sellOrder sellOrders =
    ST.stToIO $ do
        log "Building graph..."
        (graphInfo, mGraph) <- buildGraph sellOrders
        let vertexCount = length (giVertices graphInfo)
            edgeCount = giEdgeCount graphInfo
        log $ "Vertex count: " ++ show vertexCount
        log $ "Edge count:   " ++ show edgeCount
        -- Arbitrages
        buyGraph <- runArb mGraph $ do
            log "Finding arbitrages..."
            -- Asks
            (_, arbsSell) <- arbitrages sellOrder
            (buyGraph, arbsBuy) <- arbitrages buyOrder
            let arbs = arbsSell ++ arbsBuy
            -- Finds all arbitrages (regardless of "src" vertex)
            log $ unlines ["Arbitrages:", pp arbs]
            return buyGraph
        -- Match
        runMatch buyGraph $ do
            log "Matching sell order..."
            bids <- map invertSomeSellOrder <$> match sellOrder
            log "Matching buy order..."
            asks <- match buyOrder
            return (bids, asks)
