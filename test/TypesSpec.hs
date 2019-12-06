module TypesSpec where

import Test.Hspec

import Types(Color(..), swapColor)

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do

  describe "Validate swapColor" $ do

    it "swapColor is supposed to swap black" $ do
      swapColor Black `shouldBe` White

    it "swapColor is supposed to swap white" $ do
      swapColor White `shouldBe` Black
