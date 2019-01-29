{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import           Protolude                                  (lefts, rights, toS, forM_)
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import qualified OrderBook.Graph.Build                      as Lib
import qualified OrderBook.Graph.Query                      as Lib
import qualified OrderBook.Types                            as OB
import qualified CryptoVenues.Types.AppM                    as AppM
import qualified CryptoDepth.Fetch                          as Fetch
import qualified CryptoDepth.Internal.Types                 as IT

import qualified Control.Logging                            as Log
import qualified Data.Graph.Immutable                       as GI
import qualified Money
import qualified Data.Text                                  as T
import qualified Data.Vector                                as Vec
import           Data.String                                (fromString)
import           GHC.TypeLits                               (KnownSymbol, symbolVal)
import           Data.Proxy                                 (Proxy(..))
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Client.TLS                    as HTTPS
import qualified Criterion


numObLimit :: Word
numObLimit = 10

logLevel :: Log.LogLevel
logLevel = Log.LevelInfo

maxRetries :: Word
maxRetries = 10

type Numeraire = "USD"

allSellOrders :: AppM.AppM IO [SomeSellOrder]
allSellOrders = do
    booksE <- Fetch.allBooks (Proxy :: Proxy Numeraire) numObLimit
    let books = concat $ rights booksE
        errors = lefts booksE
    forM_ errors (Log.warn' . toS . show)
    return $ concatMap fromABook books
  where
    fromABook (IT.ABook ob) = fromOB ob

main :: IO ()
main = withLogging $ do
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    orders <- throwErrM $ AppM.runAppM man maxRetries $ allSellOrders
    let buildGraphQuery sellOrders' = do
            someGraph <- GI.create $ \mGraph -> Lib.build mGraph sellOrders'
            let findPath graph = Lib.query (Lib.derive graph) "BTC" numeraire
                path = GI.with someGraph findPath
            print path
    let benchmarkable = Criterion.perBatchEnv (const $ return orders) buildGraphQuery
    Criterion.benchmark benchmarkable
  where
    numeraire = fromString $ symbolVal (Proxy :: Proxy Numeraire)
    throwErrM ioA = ioA >>= either (error . show) return

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T:%3q"
    ioa


-- #### Order(book) conversion

-- | Convert all orders in an orderbook (consisting of both sell orders and buy orders)
--    into a list of sell orders
fromOB
    :: forall venue base quote.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => OB.OrderBook venue base quote
    -> [SomeSellOrder]
fromOB OB.OrderBook{..} =
    map (fromSellOrder venue) (Vec.toList $ OB.sellSide obAsks)
    ++ map (fromSellOrder venue) (map OB.invert . Vec.toList $ OB.buySide obBids)
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
