-- TODO: import from "orderbook" library somehow (currently in "test" dir)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Property.Orphans where

import MyPrelude                             hiding (NonEmpty)
import OrderBook.Types                       hiding (midPrice)
import qualified Money
import qualified Test.QuickCheck             as QC
import qualified Data.Vector                 as Vec
import Test.SmallCheck.Series                hiding (NonEmpty)
import qualified Test.SmallCheck.Series      as SS


suchThat :: Series m a -> (a -> Bool) -> Series m a
suchThat s p = s >>= \x -> if p x then pure x else empty

instance (KnownSymbol base, KnownSymbol quote, Monad m) =>
   Serial m (OrderBook venue base quote) where
      series = do
         midPrice <- series
         let buyOrderProp o  = oPrice o < midPrice
             sellOrderProp o = oPrice o > midPrice
         buyOrders'  <- SS.series `suchThat` all buyOrderProp
         sellOrders' <- SS.series `suchThat` all sellOrderProp
         return $ OrderBook (BuySide . Vec.fromList $ sortBy (comparing Down) buyOrders')
                            (SellSide . Vec.fromList $ sort sellOrders')

newtype NonEmpty a = NonEmpty a deriving (Eq, Show)

instance (KnownSymbol base, KnownSymbol quote, Monad m) =>
   Serial m (NonEmpty (OrderBook venue base quote)) where
      series = do
         midPrice <- series
         let buyOrderProp o  = oPrice o < midPrice
             sellOrderProp o = oPrice o > midPrice
         buyOrders'  <- SS.series `suchThat` all buyOrderProp `suchThat` (not . null)
         sellOrders' <- SS.series `suchThat` all sellOrderProp `suchThat` (not . null)
         return $ NonEmpty $ OrderBook (BuySide . Vec.fromList $ sortBy (comparing Down) buyOrders')
                                       (SellSide . Vec.fromList $ sort sellOrders')


instance (KnownSymbol base, KnownSymbol quote, Monad m) =>
   Serial m (Order base quote) where
      series = do
         Positive qty <- series
         price        <- series
         return $ Order qty price

instance (KnownSymbol base, KnownSymbol quote, Monad m) =>
   Serial m (Money.ExchangeRate base quote) where
      series = do
         Positive (rat :: Rational) <- series
         let msg = "Positive Rational isn't positive: " <> toS (show rat)
         return $ fromMaybe (error msg) (Money.exchangeRate rat)

instance (KnownSymbol symbol, Monad m) =>
   Serial m (Money.Dense symbol)

instance (KnownSymbol symbol, Monad m) =>
   Serial m (Positive (Money.Dense symbol)) where
      series = do
         Positive (rat :: Rational) <- series
         return $ Positive (Money.dense' rat)


instance (Show a, Serial m a) => Serial m (Vector a) where
   series = fmap Vec.fromList series

instance (KnownSymbol base, KnownSymbol quote) =>
            QC.Arbitrary (OrderBook venue base quote) where
   arbitrary = do
      midPrice   <- QC.arbitrary
      buyOrders'  <- QC.listOf $ QC.arbitrary `QC.suchThat` (\o -> oPrice o < midPrice)
      sellOrders' <- QC.listOf $ QC.arbitrary `QC.suchThat` (\o -> oPrice o > midPrice)
      return $ OrderBook (BuySide $ Vec.fromList (sortBy (comparing Down) buyOrders'))
                         (SellSide $ Vec.fromList (sort sellOrders'))

instance QC.Arbitrary (Order base quote) where
   arbitrary = Order <$> QC.arbitrary `QC.suchThat` (> Money.dense' 0)
                     <*> QC.arbitrary
