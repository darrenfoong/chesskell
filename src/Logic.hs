module Logic
  ( isInCheckmate,
    isInCheck,
    genMove,
  )
where

import Board (advanceBoard, genPossibleMoves, getKingPosition, isPositionUnderAttack)
import Data.Either (fromRight)
import Minimax (minimax, negInfinity, posInfinity)
import System.Random
import Types (Board, Color (..), Move)
import Utils (shuffle)

isInCheckmate :: Board -> Color -> Bool
isInCheckmate board color = all (\b -> isInCheck b color) ((:) board $ map snd $ genNextMoveBoards board color)

isInCheck :: Board -> Color -> Bool
isInCheck board color = maybe False (isPositionUnderAttack board color) (getKingPosition board color)

genMove :: (Board -> Color -> Int) -> StdGen -> Board -> Color -> (StdGen, Maybe Move)
genMove boardScorer gen board color =
  let (gen1, gen2) = split gen
   in case minimax boardScorer genPossibleNonCheckMoves (shuffle gen1) board color color 4 negInfinity posInfinity True of
        Nothing -> (gen2, Nothing)
        Just (_, m) -> (gen2, Just m)

-- TODO Add generation of castling moves here
genPossibleNonCheckMoves :: Board -> Color -> [Move]
genPossibleNonCheckMoves board color = map fst $ filter (\(_, b) -> not $ isInCheck b color) $ genNextMoveBoards board color

genNextMoveBoards :: Board -> Color -> [(Move, Board)]
genNextMoveBoards board color =
  let possibleMoves = genPossibleMoves board color
      possibleMoveBoards = map (\m -> (m, fromRight [] $ advanceBoard board color m)) possibleMoves
   in filter (\(_, b) -> b /= []) possibleMoveBoards
