{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Prelude
import           Protolude                                  (lefts, rights, toS, forM_, void, (%))
import qualified OrderBook.Graph.Internal.Util              as Util
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import qualified OrderBook.Graph                            as Lib
import qualified CryptoVenues.Types.AppM                    as AppM
import           CryptoVenues.Types.ABook                   (ABook(ABook))
import qualified CryptoVenues.Fetch.Debug                   as Fetch

import qualified Control.Monad.ST                           as ST
import qualified Control.Logging                            as Log
import qualified Data.Graph.Digraph                         as DG
import qualified Data.Text                                  as T
import           Data.List                                  (sortBy)
import           Data.Ord                                   (comparing)

import           Data.Proxy                                 (Proxy(..))
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Client.TLS                    as HTTPS
import qualified Criterion
import qualified Data.Aeson                                 as Json
import           Data.Aeson                                 ((.=))
import           System.Directory                           (listDirectory)
import           System.Environment                         (getArgs)


main :: IO ()
main = do
    [fileName] <- getArgs
    orders <- readOrdersFile fileName
    putStrLn $ "Order count: " ++ show (length orders)
    marketDepthWriteFile "/Users/runesvendsen/code/order-graph/web/btcusd.json" orders

readOrdersFile :: FilePath -> IO [SomeSellOrder]
readOrdersFile filePath = do
    books <- decodeFileOrFail filePath
    return $ concatMap fromABook (books :: [ABook])
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file

fetchOrders :: IO [SomeSellOrder]
fetchOrders = withLogging $ do
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    throwErrM $ AppM.runAppM man maxRetries $ allSellOrders
  where
    throwErrM ioA = ioA >>= either (error . show) return

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T:%3q"
    ioa

marketDepthWriteFile
    :: FilePath
    -> [SomeSellOrder]
    -> IO ()
marketDepthWriteFile obPath sellOrders = do
    (bids, asks) <- ST.stToIO $ DG.withGraph $ \mGraph -> do

        Lib.build mGraph sellOrders
        asks <- Lib.match mGraph asksOrder
        bids <- map Lib.invertSomeSellOrder <$> Lib.match mGraph bidsOrder
        return (bids, asks)
    let trimmedAsks = trimOrders $ sortBy (comparing soPrice)        asks
        trimmedBids = trimOrders $ sortBy (flip $ comparing soPrice) bids
    let jsonOB = Json.object
            [ "bids" .= map toJson trimmedBids
            , "asks" .= map toJson trimmedAsks
            ]
    Json.encodeFile obPath jsonOB
    putStrLn $ "Wrote " ++ show obPath
  where
    trimOrders :: [SomeSellOrder] -> [SomeSellOrder]
    trimOrders = Util.compress 500 . Util.merge . Util.trimSlippage (50%1)
    asksOrder :: Lib.BuyOrder "BTC" "USD"
    asksOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    bidsOrder :: Lib.BuyOrder "USD" "BTC"
    bidsOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    toJson :: SomeSellOrder -> Json.Value
    toJson sso = Json.toJSON -- format: ["0.03389994", 34.14155996]
        ( show (realToFrac $ soPrice sso :: Double)
        , realToFrac $ soQty sso :: Double
        )

getTestFiles :: IO [FilePath]
getTestFiles = do
    jsonFiles <- filter jsonExtension <$> listDirectory testDataDir
    return $ map (testDataDir ++) jsonFiles
  where
    testDataDir = "test/data/"
    jsonExtension fileName = let splitByDot = T.split (== '.') (toS fileName) in
        if null splitByDot
            then False
            else last splitByDot == "json"

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

fromABook :: ABook -> [SomeSellOrder]
fromABook (ABook ob) = Util.fromOB ob

doEverything :: [SomeSellOrder] -> IO ()
doEverything orders = do
    putStrLn $ "Sell order count: " ++ show (length orders)
    let benchmarkable = Criterion.perBatchEnv (const $ return orders) buildGraphQuery
    Criterion.benchmark benchmarkable
  where
    buyOrder :: Lib.BuyOrder "BTC" Numeraire
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    buildGraphQuery sellOrders' = do
            vertexCount <- ST.stToIO $ DG.withGraph
                $ \mGraph -> do
                    Lib.build mGraph sellOrders'
                    void $ Lib.match mGraph buyOrder
                    DG.vertexCount mGraph
            putStrLn $ "Symbol count: " ++ show vertexCount
