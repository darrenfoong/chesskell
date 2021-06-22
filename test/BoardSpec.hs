module BoardSpec where

import Board (advanceBoard, readBoard, writeBoard)
import Data.Either (fromLeft, fromRight)
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
            \........\
            \........\
            \........\
            \........\
            \PPPPPPPP\
            \RNBQKBNR"
      let deserializedBoard = fromRight [] $ readBoard $ pack boardStr
      writeBoard deserializedBoard `shouldBe` boardStr

  describe "advanceBoard" $ do
    it "accepts valid castling (black short)" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "rnbqkbnr\
                  \pppppppp\
                  \........\
                  \........\
                  \........\
                  \........\
                  \PPPPPPPP\
                  \RNBQK..R"
      let advancedBoard = fromRight [] $ advanceBoard board Black ((5, 8), (7, 8))
      writeBoard advancedBoard
        `shouldBe` "rnbqkbnr\
                   \pppppppp\
                   \........\
                   \........\
                   \........\
                   \........\
                   \PPPPPPPP\
                   \RNBQ.SL."

    it "accepts valid castling (black long)" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "rnbqkbnr\
                  \pppppppp\
                  \........\
                  \........\
                  \........\
                  \........\
                  \PPPPPPPP\
                  \R...KBNR"
      let advancedBoard = fromRight [] $ advanceBoard board Black ((5, 8), (3, 8))
      writeBoard advancedBoard
        `shouldBe` "rnbqkbnr\
                   \pppppppp\
                   \........\
                   \........\
                   \........\
                   \........\
                   \PPPPPPPP\
                   \..LS.BNR"

    it "accepts valid castling (white short)" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "rnbqk..r\
                  \pppppppp\
                  \........\
                  \........\
                  \........\
                  \........\
                  \PPPPPPPP\
                  \RNBQKBNR"
      let advancedBoard = fromRight [] $ advanceBoard board White ((5, 1), (7, 1))
      writeBoard advancedBoard
        `shouldBe` "rnbq.sl.\
                   \pppppppp\
                   \........\
                   \........\
                   \........\
                   \........\
                   \PPPPPPPP\
                   \RNBQKBNR"

    it "accepts valid castling (white long)" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "r...kbnr\
                  \pppppppp\
                  \........\
                  \........\
                  \........\
                  \........\
                  \PPPPPPPP\
                  \RNBQKBNR"
      let advancedBoard = fromRight [] $ advanceBoard board White ((5, 1), (3, 1))
      writeBoard advancedBoard
        `shouldBe` "..ls.bnr\
                   \pppppppp\
                   \........\
                   \........\
                   \........\
                   \........\
                   \PPPPPPPP\
                   \RNBQKBNR"

    it "accepts en passant" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "rnbqkbnr\
                  \ppppppp.\
                  \........\
                  \........\
                  \......Op\
                  \........\
                  \PPPPPP.P\
                  \RNBQKBNR"
      let advancedBoard = fromRight [] $ advanceBoard board White ((8, 5), (7, 6))
      writeBoard advancedBoard
        `shouldBe` "rnbqkbnr\
                   \ppppppp.\
                   \........\
                   \........\
                   \........\
                   \......p.\
                   \PPPPPP.P\
                   \RNBQKBNR"

    it "rejects invalid en passant" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "rnbqkbnr\
                  \ppppppp.\
                  \........\
                  \........\
                  \......Pp\
                  \........\
                  \PPPPPP.P\
                  \RNBQKBNR"
      let errorMessage = fromLeft [] $ advanceBoard board White ((8, 5), (7, 6))
      errorMessage `shouldBe` "ERROR: Invalid move: Normal ((8,5),(7,6))"
