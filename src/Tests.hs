module Main where

import Test.Hspec

import Types(Color(..), swapColor)

main :: IO ()
main = hspec $ do

  describe "Validate swapColor" $ do

    it "swapColor is supposed to swap black" $ do
      swapColor Black `shouldBe` White

    it "swapColor is supposed to swap white" $ do
      swapColor White `shouldBe` Black
