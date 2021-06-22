module BoardSpec where

import Board (advanceBoard, readBoard, writeBoard)
import Data.Either (fromRight)
import Data.Text (pack)
import Test.Hspec
import Types (Color (..))

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do
  describe "writeBoard and readBoard" $ do
    it "serialize properly" $ do
      let boardStr =
            "rnbqkbnr\
            \pppppppp\
            \########\
            \########\
            \########\
            \########\
            \PPPPPPPP\
            \RNBQKBNR"
      let deserializedBoard = case readBoard $ pack boardStr of
            Left _ -> []
            Right board -> board
      writeBoard deserializedBoard `shouldBe` boardStr

  describe "advanceBoard" $ do
    it "accepts valid castling (black short)" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "rnbqkbnr\
                  \pppppppp\
                  \########\
                  \########\
                  \########\
                  \########\
                  \PPPPPPPP\
                  \RNBQK##R"
      let advancedBoard = fromRight [] $ advanceBoard board Black ((5, 8), (7, 8))
      writeBoard advancedBoard
        `shouldBe` "rnbqkbnr\
                   \pppppppp\
                   \########\
                   \########\
                   \########\
                   \########\
                   \PPPPPPPP\
                   \RNBQ#SL#"
