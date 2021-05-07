module TypesSpec where

import Test.Hspec
import Types (Color (..), swapColor)

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do
  describe "swapColor" $ do
    it "swaps Black to White" $ do
      swapColor Black `shouldBe` White

    it "swaps White to Black" $ do
      swapColor White `shouldBe` Black
