{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Protolude                                  (lefts, rights, toS, forM_, void, (%))
import qualified OrderBook.Graph.Internal.Util              as Util
import           OrderBook.Graph.Types                      (SomeSellOrder, SomeSellOrder'(..))
import qualified OrderBook.Graph                            as Lib
import qualified OrderBook.Types                            as OB
import qualified CryptoVenues.Types.AppM                    as AppM
import qualified CryptoDepth.Fetch                          as Fetch
import qualified CryptoDepth.Internal.Types                 as IT

import qualified Control.Logging                            as Log
import qualified Data.Graph.Immutable                       as GI
import qualified Data.Text                                  as T

import           Data.Proxy                                 (Proxy(..))
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Client.TLS                    as HTTPS
import qualified Criterion
import qualified Data.Aeson                                 as Json
import           Data.Aeson                                 ((.=))
import           System.Directory                           (listDirectory)


-- main' :: IO ()
-- main' = do
--     booksList <- mapM decodeFileOrFail =<< getTestFiles
--     forM_ booksList $ \books -> do
--         let orders = concatMap fromABook (books :: [IT.ABook])
--         doEverything orders
--   where
--     fromABook (IT.ABook ob) = fromOB ob
--     throwError file str = error $ file ++ ": " ++ str
--     decodeFileOrFail file =
--         either (throwError file) return =<< Json.eitherDecodeFileStrict file

marketDepthWriteFile
    :: FilePath
    -> [SomeSellOrder]
    -> IO ()
marketDepthWriteFile obPath sellOrders =
    void $ GI.create $ \mGraph -> do
        Lib.build mGraph sellOrders
        putStrLn "#################### ASKS ####################"
        asks <- Lib.match mGraph asksOrder
        putStrLn "#################### BIDS ####################"
        bids <- Lib.match mGraph bidsOrder
        -- TEST
        -- putStrLn "Asserting sorted bids/asks..."
        -- Util.assertAscendingPriceSorted asks
        -- Util.assertAscendingPriceSorted bids
        let trimmedAsks = trimOrders asks
            trimmedBids = trimOrders bids
        -- putStrLn "Asserting sorted trimmed{bids/asks}..."
        -- Util.assertAscendingPriceSorted trimmedAsks
        -- Util.assertAscendingPriceSorted trimmedBids
        -- end TEST
        let jsonOB = Json.object
              [ "bids" .= map toJson (map Lib.invertSomeSellOrder trimmedBids)
              , "asks" .= map toJson trimmedAsks
              ]
        Json.encodeFile obPath jsonOB
        putStrLn $ "Wrote " ++ show obPath
  where
    trimOrders :: [SomeSellOrder] -> [SomeSellOrder]
    trimOrders = Util.merge . Util.trimSlippage (50%1)
    asksOrder :: Lib.BuyOrder "BTC" "USD"
    asksOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    bidsOrder :: Lib.BuyOrder "USD" "BTC"
    bidsOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    toJson :: SomeSellOrder -> Json.Value
    toJson sso = Json.toJSON -- format: ["0.03389994", 34.14155996]
        ( show (realToFrac $ soPrice sso :: Double)
        , realToFrac $ soQty sso :: Double
        )

main = withLogging $ do
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    orders <- throwErrM $ AppM.runAppM man maxRetries $ allSellOrders
    marketDepthWriteFile "/Users/runesvendsen/code/order-graph/web/btcusd.json" orders
  where
    throwErrM ioA = ioA >>= either (error . show) return

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
  where
    fromABook (IT.ABook ob) = Util.fromOB ob

doEverything :: [SomeSellOrder] -> IO ()
doEverything orders = do
    putStrLn $ "Sell order count: " ++ show (length orders)
    let benchmarkable = Criterion.perBatchEnv (const $ return orders) buildGraphQuery
    Criterion.benchmark benchmarkable
  where
    buyOrder :: Lib.BuyOrder "BTC" Numeraire
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    buildGraphQuery sellOrders' =
            void $ GI.create $ \mGraph -> do
                Lib.build mGraph sellOrders'
                GI.freeze mGraph >>= \g -> putStrLn $ "Symbol count: " ++ show (GI.sizeInt $ GI.size g)
                void $ Lib.match mGraph buyOrder

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T:%3q"
    ioa
