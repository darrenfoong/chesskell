module PositionSpec where

import Position (mkPos, validPos)
import Test.Hspec

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do
  describe "mkPos" $ do
    it "converts (e, 2) to (5, 2)" $ do
      mkPos ('e', '2') `shouldBe` (5, 2)

  describe "validPos" $ do
    it "marks (1, 1) as valid" $ do
      (1, 1) `shouldSatisfy` validPos

    it "marks (9, 14) as invalid" $ do
      (9, 14) `shouldNotSatisfy` validPos
