{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Property.Combine
( spec )
where

import           OrderBook.Graph.Internal.Prelude           hiding (NonEmpty)
import qualified OrderBook.Graph.Internal.Util              as Util

import           Test.Hspec
import qualified Test.Hspec.SmallCheck                      as SC
import qualified Test.SmallCheck.Series                     as Series


spec :: Spec
spec = do
    describe "combine" $ do
        it "(const $ const Nothing) = id" $
            SC.property $ \lst ->
                Util.combine (\_ _ -> Nothing) lst `shouldBe` (lst :: [Int])
        it "(const $ const $ Just value) (NonEmpty _) = [value]" $
            let value = 1 :: Int in
                SC.property $ \(Series.NonEmpty lst) ->
                    when (length lst >= 2) $
                        Util.combine (\_ _ -> Just value) lst `shouldBe` [value]
