{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Property.Build
( spec )
where

import qualified OrderBook.Graph.Build                      as Build
import qualified Data.Graph.Digraph                         as DG

import           Test.Hspec
import qualified Test.Hspec.SmallCheck                      as SC
import qualified Test.SmallCheck.Series                     as SS
import qualified Control.Monad.ST                           as ST
import           Control.Monad                              (foldM)
import           Data.List                                  (sort)
import           Data.String                                (fromString)
import qualified System.Random.Shuffle                      as Shuffle


instance Monad m => SS.Serial m Build.SomeSellOrder where
    series =
        Build.SomeSellOrder'
            <$> return 1.0
            <*> return 1.0
            <*> (fromString <$> SS.series)
            <*> (fromString <$> SS.series)
            <*> return "TestVenue"

spec :: Spec
spec = describe "Build" $ do
    describe "built graph" $ do
        it "contains all input orders" $
            SC.property addEdgesCheckOutgoing

addEdgesCheckOutgoing
    :: [Build.SomeSellOrder]
    -> Expectation
addEdgesCheckOutgoing orders = do
    shuffledOrders <- Shuffle.shuffleM orders
    outgoingEdges <- ST.stToIO $ DG.withGraph $ \graph -> do
        Build.buildFromOrders graph shuffledOrders
        foldM (collectOutgoing graph) [] =<< DG.vertices graph
    let graphOrders = concat $ Build.toList . Build.unTagged <$> concat outgoingEdges
    sort graphOrders `shouldBe` sort orders
  where
    collectOutgoing graph accum vertex = do
        outEdges <- DG.outgoingEdges graph vertex
        return $ outEdges : accum
