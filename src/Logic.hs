module Logic
  ( genMove,
    isInCheckmate,
    isInCheck,
    promotePawns,
    respondBoard,
  )
where

import Board (advanceBoard, getPiece, mkCoords, validMove)
import Data.Either (fromRight)
import Minimax (minimax, negInfinity, posInfinity)
import System.Random
import Types (Board, CPiece (..), Color (..), Move, Piece (..), Position, swapColor)
import Utils (shuffle)

isInCheckmate :: Color -> Board -> Bool
isInCheckmate color board = all (isInCheck color) ((:) board $ map snd $ genNextMoveBoards board color)

isInCheck :: Color -> Board -> Bool
isInCheck color board = maybe False (isUnderAttack color board) (getKingPosition color board)

isUnderAttack :: Color -> Board -> Position -> Bool
isUnderAttack color board pos = elem pos $ map snd $ genMoves board $ swapColor color

getKingPosition :: Color -> Board -> Maybe Position
getKingPosition color board = case filter (\p -> getPiece board p == CP color King) (genPositions board color) of
  [] -> Nothing
  p : _ -> Just p

promotePawn :: [CPiece] -> [CPiece]
promotePawn [] = []
promotePawn (CP color Pawn : ps) = CP color Queen : promotePawn ps
promotePawn (p : ps) = p : promotePawn ps

promotePawns :: Board -> Board
promotePawns board =
  let firstRow = head board
      lastRow = board !! 7
   in promotePawn firstRow : take 6 (tail board) ++ [promotePawn lastRow]

respondBoard :: (Color -> Board -> Int) -> StdGen -> Board -> Color -> (StdGen, Either String Board)
respondBoard boardScorer gen board color =
  let (newGen, mMove) = genMove boardScorer gen board color
   in case mMove of
        Just m -> (newGen, advanceBoard board m color)
        Nothing -> (newGen, Left "ERROR: Program has made an invalid move")

genMove :: (Color -> Board -> Int) -> StdGen -> Board -> Color -> (StdGen, Maybe Move)
genMove boardScorer gen board color =
  let (gen1, gen2) = split gen
   in case minimax boardScorer genNonCheckMoves (shuffle gen1) board color color 4 negInfinity posInfinity True of
        Nothing -> (gen2, Nothing)
        Just (_, m) -> (gen2, Just m)

genNextMoveBoards :: Board -> Color -> [(Move, Board)]
genNextMoveBoards board color =
  let possibleMoves = genMoves board color
      possibleMoveBoards = map (\m -> (m, fromRight [] $ advanceBoard board m color)) possibleMoves
   in filter (\(_, b) -> b /= []) possibleMoveBoards

genNonCheckMoves :: Board -> Color -> [Move]
genNonCheckMoves board color = map fst $ filter (\(_, b) -> not $ isInCheck color b) $ genNextMoveBoards board color

genMoves :: Board -> Color -> [Move]
genMoves board color = concatMap (genPossibleMovesPiece board) (genPositions board color)

genPositions :: Board -> Color -> [Position]
genPositions board color =
  foldl
    ( \ps p -> case getPiece board p of
        CP clr _ -> if color == clr then p : ps else ps
        _ -> ps
    )
    []
    mkCoords

genPossibleMovesPiece :: Board -> Position -> [Move]
genPossibleMovesPiece board position = case getPiece board position of
  CP color _ -> filter (\move -> validMove board move color) (map (\pos -> (position, pos)) mkCoords)
  _ -> []
