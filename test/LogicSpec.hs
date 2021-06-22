module LogicSpec where

import Board (readBoard)
import Data.Either (fromRight)
import Data.Text (pack)
import Logic (isInCheckmate)
import Test.Hspec
import Types (Color (Black))

{- HLINT ignore "Redundant do" -}
spec :: Spec
spec = do
  describe "isInCheckmate" $ do
    it "detects checkmate for Black King" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "........\
                  \........\
                  \........\
                  \........\
                  \........\
                  \.....q.r\
                  \........\
                  \.....r.K"
      isInCheckmate board Black `shouldBe` True

    it "detects checkmate for Black King" $ do
      let board =
            fromRight [] $
              readBoard $
                pack
                  "rnbqk..r\
                  \pPpp.ppp\
                  \........\
                  \....p...\
                  \......n.\
                  \........\
                  \RPPPPbPP\
                  \.NBQKBNR"
      isInCheckmate board Black `shouldBe` True
