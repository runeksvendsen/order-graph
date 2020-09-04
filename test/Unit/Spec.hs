{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Unit.Spec
( tests )
where

import           Common.Util                        (assertMatchedOrders, shouldBeIgnoringVenue)

import qualified OrderBook.Graph                    as Lib
import qualified OrderBook.Graph.Internal.Util      as Util

import           Test.HUnit
import           Test.Hspec.Expectations.Pretty


tests :: Test
tests = TestList
  [ TestLabel "single order"  singleOrder
  , TestLabel "simple graph" simpleGraph
  , TestLabel "Util.merge 2" merge2
  , TestLabel "Util.merge 3A" merge3A
  , TestLabel "Util.merge 3B" merge3B
  ]

singleOrder :: Test
singleOrder =
    TestCase $ assertMatchedOrders [testOrder] buyOrder
        (`shouldBeIgnoringVenue` Util.merge [testOrder])
  where
    buyOrder :: Lib.BuyOrder "BTC" "USD"
    buyOrder = Lib.unlimited
    testOrder :: Lib.SomeSellOrder
    testOrder = Lib.SomeSellOrder'
        { soPrice = 100
        , soQty   = 20
        , soBase  = "BTC"
        , soQuote = "USD"
        , soVenue = "test"
        }

-- Example graph: img/otplk.png
simpleGraph :: Test
simpleGraph =
    TestCase $ assertMatchedOrders sellOrders buyOrder
        (`shouldBeIgnoringVenue` expectedOrders)
  where
    buyOrder :: Lib.BuyOrder "C" "A"
    buyOrder = Lib.unlimited
    expectedOrders =
        [ aToCOrder 1 1         -- fromAtoB_1 -> fromBtoC_1
        , fromAtoC_3
        , aToCOrder 4 (1/2)     -- fromAtoB_2 -> fromBtoC_2
        , fromAtoC_5
        ]
    sellOrders =
        [ fromAtoB_1
        , fromAtoB_2
        , fromBtoC_1
        , fromBtoC_2
        , fromAtoC_3
        , fromAtoC_5
        ]
    fromAtoB_1 = aToBOrder 1 1
    fromAtoB_2 = aToBOrder 2 1
    fromBtoC_1 = bToCOrder 1 1
    fromBtoC_2 = bToCOrder 2 1
    fromAtoC_3 = aToCOrder 3 (1/3)
    fromAtoC_5 = aToCOrder 5 (1/5)
    aToBOrder price qty = Lib.SomeSellOrder'
        { soPrice = price
        , soQty   = qty
        , soBase  = "B"
        , soQuote = "A"
        , soVenue = "test"
        }
    bToCOrder price qty = Lib.SomeSellOrder'
        { soPrice = price
        , soQty   = qty
        , soBase  = "C"
        , soQuote = "B"
        , soVenue = "test"
        }
    aToCOrder price qty = Lib.SomeSellOrder'
        { soPrice = price
        , soQty   = qty
        , soBase  = "C"
        , soQuote = "A"
        , soVenue = "test"
        }


merge2 :: Test
merge2 =
    TestCase $ Util.merge inputOrders `shouldBe` outputOrders
  where
    templateOrder = Lib.SomeSellOrder'
        { soQty   = 0 :: Int
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
        { soQty   = 0 :: Int
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
        { soQty   = 0 :: Int
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
