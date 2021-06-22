module LogicSpec where

import Board (mkBoard, readBoard)
import Data.Text (pack)
import Logic (isInCheckmate)
import Test.Hspec
import Types (Color (Black))

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do
  describe "isInCheckmate" $ do
    it "detects checkmate for Black King" $ do
      let board = case readBoard $ pack "#############################################q#r#############r#K" of
            Left _ -> mkBoard
            Right b -> b
      isInCheckmate board Black `shouldBe` True

    it "detects checkmate for Black King" $ do
      let board = case readBoard $ pack "rnbqk##rpPpp#ppp############p#########n#########RPPPPbPP#NBQKBNR" of
            Left _ -> mkBoard
            Right b -> b
      isInCheckmate board Black `shouldBe` True
