module BoardSpec where

import Board (mkBoard, readBoard, writeBoard)
import Data.Text (pack)
import Test.Hspec

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do
  describe "writeBoard and readBoard" $ do
    it "serialize properly" $ do
      let deserializedBoard = case readBoard $ pack $ writeBoard mkBoard of
            Left _ -> []
            Right board -> board
      writeBoard deserializedBoard `shouldBe` writeBoard mkBoard
