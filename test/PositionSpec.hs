module PositionSpec where

import Test.Hspec

import Position(mkPos)

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do

  describe "Validate mkPos" $ do

    it "(e, 2) is supposed to be (5, 2)" $ do
      mkPos ('e', '2') `shouldBe` (5, 2)
