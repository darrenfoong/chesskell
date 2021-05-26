module Logic
  ( genMove,
    isInCheckmate,
    isInCheck,
    respondBoard,
  )
where

import Board (advanceBoard, genPossibleMoves, getKingPosition, isPositionUnderAttack)
import Data.Either (fromRight)
import Minimax (minimax, negInfinity, posInfinity)
import System.Random
import Types (Board, Color (..), Move)
import Utils (shuffle)

isInCheckmate :: Color -> Board -> Bool
isInCheckmate color board = all (isInCheck color) ((:) board $ map snd $ genNextMoveBoards board color)

isInCheck :: Color -> Board -> Bool
isInCheck color board = maybe False (isPositionUnderAttack color board) (getKingPosition color board)

respondBoard :: (Color -> Board -> Int) -> StdGen -> Board -> Color -> (StdGen, Either String Board)
respondBoard boardScorer gen board color =
  let (newGen, mMove) = genMove boardScorer gen board color
   in case mMove of
        Just m -> (newGen, advanceBoard board m color)
        Nothing -> (newGen, Left "ERROR: Program has made an invalid move")

genMove :: (Color -> Board -> Int) -> StdGen -> Board -> Color -> (StdGen, Maybe Move)
genMove boardScorer gen board color =
  let (gen1, gen2) = split gen
   in case minimax boardScorer genPossibleNonCheckMoves (shuffle gen1) board color color 4 negInfinity posInfinity True of
        Nothing -> (gen2, Nothing)
        Just (_, m) -> (gen2, Just m)

genPossibleNonCheckMoves :: Board -> Color -> [Move]
genPossibleNonCheckMoves board color = map fst $ filter (\(_, b) -> not $ isInCheck color b) $ genNextMoveBoards board color

genNextMoveBoards :: Board -> Color -> [(Move, Board)]
genNextMoveBoards board color =
  let possibleMoves = genPossibleMoves board color
      possibleMoveBoards = map (\m -> (m, fromRight [] $ advanceBoard board m color)) possibleMoves
   in filter (\(_, b) -> b /= []) possibleMoveBoards
