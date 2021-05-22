{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Unit.Regression
( tests )
where

import qualified OrderBook.Graph as Lib
import qualified OrderBook.Graph.Test.Data as TestData
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Control.Monad.ST as ST
import           Test.HUnit
import           Test.Hspec.Expectations.Pretty


matchOrders :: FilePath -> Double -> Lib.Currency -> Lib.Currency -> IO ([Lib.SellPath], [Lib.BuyPath])
matchOrders file slippage numeraire crypto = do
    return $ ST.runST $ Lib.buildBuyGraph noLogging (toRational slippage) TestData.orderbookList >>=
            Lib.matchOrders noLogging numeraire crypto . snd

toHumanReadable :: ([Lib.SellPath], [Lib.BuyPath]) -> ([(Double, String)], [(Double, String)])
toHumanReadable (sellPaths, buyPaths) =
    (map showIt $ mkData sellPaths, map showIt $ mkData buyPaths)
  where
    mkData paths = toList $ NE.map dropPriceRange . NE.sortWith quantity . Lib.liPaths
        <$> (Lib.toSideLiquidity <$> NE.nonEmpty paths)
    quantity (qty, _, _) = qty
    dropPriceRange (qty, _, path) = (qty, path)
    toList Nothing = []
    toList (Just ne) = NE.toList ne
    showIt (qty, path) = (fromRational qty :: Double, T.unpack (Lib.showPath path))

tests :: Test
tests = TestLabel "regression" $ TestList $
    [ TestLabel "test/data/double/test19.json BTC USD 0.5" $ TestList
        [ TestLabel "FULL: test/data/double/test19.json BTC USD 0.5" $ TestCase $
            outputCompare "test/data/double/test19.json" "test/data/regression/double-test19.txt" 0.5 "USD" "BTC"
        , TestLabel "sell/buy: " $ TestCase $ do
            liquidityInfoM <- Lib.toLiquidityInfo <$> matchOrders "test/data/double/test19.json" 0.5 "USD" "BTC"
            fmap buySellLiquidity liquidityInfoM `shouldBe` Just (Just 51175354, Just 23781286)
        ]
    ]
  where
    buySellLiquidity :: Lib.LiquidityInfo -> (Maybe Integer, Maybe Integer)
    buySellLiquidity Lib.LiquidityInfo{..} =
        (fmap sideLiquidityFloor liBuyLiquidity, fmap sideLiquidityFloor liSellLiquidity)
    sideLiquidityFloor = floor . Lib.liLiquidity
    outputCompare inputFile expectedOutFile slippage numeraire crypto = do
        outLines <- toHumanReadable <$> matchOrders inputFile slippage numeraire crypto
        outLines `shouldBe` TestData.regressData

noLogging :: Monad m => String -> m ()
noLogging = const $ return ()
