module PositionSpec where

import Position (isValidPosition, mkPosition, mkPositions)
import Test.Hspec

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do
  describe "mkPosition" $ do
    it "converts (e, 2) to (5, 2)" $ do
      mkPosition ('e', '2') `shouldBe` (5, 2)

  describe "isValidPosition" $ do
    it "marks (1, 1) as valid" $ do
      (1, 1) `shouldSatisfy` isValidPosition

    it "marks (9, 14) as invalid" $ do
      (9, 14) `shouldNotSatisfy` isValidPosition

  describe "mkPositions" $ do
    it "makes correct positions between (1, 1) and (1, 1)" $ do
      mkPositions (1, 1) (1, 1) `shouldBe` []

    it "makes correct positions between (1, 1) and (2, 2)" $ do
      mkPositions (1, 1) (2, 2) `shouldBe` []

    it "makes correct positions between (1, 1) and (4, 4)" $ do
      mkPositions (1, 1) (4, 4) `shouldBe` [(2, 2), (3, 3)]

    it "makes correct positions between (4, 4) and (1, 1)" $ do
      mkPositions (4, 4) (1, 1) `shouldBe` [(3, 3), (2, 2)]

    it "makes correct positions between (0, 0) and (0, 4)" $ do
      mkPositions (0, 0) (0, 4) `shouldBe` [(0, 1), (0, 2), (0, 3)]

    it "makes correct positions between (3, 8) and (1, 6)" $ do
      mkPositions (3, 8) (1, 6) `shouldBe` [(2, 7)]
