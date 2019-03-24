{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Unit.Spec
( tests )
where

import           OrderBook.Graph.Internal.Prelude
import           Common.Util                        (assertMatchedOrders)

import qualified OrderBook.Graph                    as Lib
import qualified OrderBook.Graph.Internal.Util      as Util
import qualified Data.Graph.Immutable               as GI

import           Test.HUnit
import qualified Data.List.NonEmpty                 as NE
import           Test.Hspec.Expectations.Pretty


tests :: Test
tests = TestList
  [ TestLabel "single order"  singleOrder
  , TestLabel "Util.merge 2" merge2
  , TestLabel "Util.merge 3A" merge3A
  , TestLabel "Util.merge 3B" merge3B
  ]

singleOrder :: Test
singleOrder =
    TestCase $ assertMatchedOrders [testOrder] buyOrder [testOrder]
  where
    buyOrder :: Lib.BuyOrder "BTC" "USD"
    buyOrder = Lib.BuyOrder' 1.0 Nothing Nothing
    testOrder :: Lib.SomeSellOrder
    testOrder = Lib.SomeSellOrder'
        { soPrice = 100
        , soQty   = 20
        , soBase  = "BTC"
        , soQuote = "USD"
        , soVenue = "test"
        }


merge2 :: Test
merge2 =
    TestCase $ Util.merge inputOrders `shouldBe` outputOrders
  where
    templateOrder = Lib.SomeSellOrder'
        { soQty   = 0
        , soPrice = 0
        , soBase  = "base"
        , soQuote = "quote"
        , soVenue = "venue"
        }
    outputOrders =
        [ templateOrder { Lib.soQty=1.0000,  Lib.soPrice=3.0000 }
        , templateOrder { Lib.soQty=11.0000, Lib.soPrice=2.0000 }
        ]
    inputOrders =
        [ templateOrder { Lib.soQty=1.0000, Lib.soPrice=3.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.5000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=0.6667, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=3.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=0.3333, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=2.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=0.5000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=2.0000 }
        ]


merge3A :: Test
merge3A =
    TestCase $ Util.merge inputOrders `shouldBe` outputOrders
  where
    templateOrder = Lib.SomeSellOrder'
        { soQty   = 0
        , soPrice = 0
        , soBase  = "base"
        , soQuote = "quote"
        , soVenue = "venue"
        }
    outputOrders =
        [ templateOrder { Lib.soQty=1.0, Lib.soPrice=3.0000 }
        , templateOrder { Lib.soQty=1.5, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=2.5, Lib.soPrice=1.5000 }
        ]
    inputOrders =
        [ templateOrder { Lib.soQty=1.0000, Lib.soPrice=3.0000 }
        , templateOrder { Lib.soQty=0.5000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=1.5000, Lib.soPrice=1.5000 }
        ]


merge3B :: Test
merge3B =
    TestCase $ Util.merge inputOrders `shouldBe` outputOrders
  where
    templateOrder = Lib.SomeSellOrder'
        { soQty   = 0
        , soPrice = 0
        , soBase  = "base"
        , soQuote = "quote"
        , soVenue = "venue"
        }
    outputOrders =
        [ templateOrder { Lib.soQty=1.0000,  Lib.soPrice=3.0000 }
        , templateOrder { Lib.soQty=11.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=11.0000, Lib.soPrice=1.5000 }
        ]
    inputOrders =
        [ templateOrder { Lib.soQty=1.0000, Lib.soPrice=3.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.5000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=0.6667, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=3.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=0.3333, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=2.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=0.5000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=2.0000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=1.5000, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=0.6667, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=3.0000, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=0.3333, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=2.0000, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=0.5000, Lib.soPrice=1.5000 }
        , templateOrder { Lib.soQty=1.0000, Lib.soPrice=1.5000 }
        ]
