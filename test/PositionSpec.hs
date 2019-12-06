module PositionSpec where

import Test.Hspec

import Position(mkPos, validPos)

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do

  describe "mkPos" $ do

    it "converts (e, 2) to (5, 2)" $ do
      mkPos ('e', '2') `shouldBe` (5, 2)

  describe "validPos" $ do

    it "marks (1, 1) as valid" $ do
      validPos (1, 1) `shouldBe` True

    it "marks (9, 14) as invalid" $ do
      validPos (9, 14) `shouldBe` False
