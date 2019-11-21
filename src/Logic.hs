module Logic
(
  respondBoard
) where

import Data.List
import System.Random

import Types (Color(..), CPiece(..), Board, Position, Move, swapColor)
import Board (scoreBoard, advanceBoard, movePiece, getPiece, mkCoords, validMove)

respondBoard :: StdGen -> Board -> Color -> (StdGen, Either String Board)
respondBoard gen board color = let (newGen, maybeMove) = genMove gen board color in
                                 case maybeMove of
                                   Just m -> (newGen, advanceBoard board m color)
                                   Nothing -> (newGen, Left "ERROR: Program has made an invalid move")

genMove :: StdGen -> Board -> Color -> (StdGen, Maybe Move)
genMove gen board color = let (_,m) = minimax board color color 3 True in
                            (gen, Just m)

minimax :: Board -> Color -> Color -> Int -> Bool -> (Int, Move)
minimax board scoringColor playerColor 1 maximising = maximumBy (compareMove maximising) $ map (\m -> (scoreBoard scoringColor (movePiece board m), m)) $ genMoves board playerColor
minimax board scoringColor playerColor n maximising = maximumBy (compareMove maximising) $ map (\m -> let (s,_) = minimax (movePiece board m) scoringColor (swapColor playerColor) (n-1) (not maximising) in (s,m)) $ genMoves board playerColor

compareMove :: Bool -> (Int, Move) -> (Int, Move) -> Ordering
compareMove maximising (s1,_) (s2,_) = if maximising then compare s1 s2 else compare s2 s1

genMoves :: Board -> Color -> [Move]
genMoves board color = concatMap (genPossibleMovesPiece board) (genPositions board color)

genPositions :: Board -> Color -> [Position]
genPositions board color = foldl (\ps p -> case getPiece board p of
                                             CP clr _ -> if color == clr then p:ps else ps
                                             _        -> ps) [] mkCoords

genPossibleMovesPiece :: Board -> Position -> [Move]
genPossibleMovesPiece board position = case getPiece board position of
                                         CP color _ -> filter (\move -> validMove board move color) (map (\pos -> (position, pos)) mkCoords)
                                         _          -> []
