module PositionSpec where

import Test.Hspec

import Position(mkPos, validPos)

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do

  describe "Validate mkPos" $ do

    it "(e, 2) is supposed to be (5, 2)" $ do
      mkPos ('e', '2') `shouldBe` (5, 2)

  describe "Validate validPos" $ do

    it "(1, 1) is supposed to be valid" $ do
      validPos (1, 1) `shouldBe` True

    it "(9, 14) is supposed to be invalid" $ do
      validPos (9, 14) `shouldBe` False
