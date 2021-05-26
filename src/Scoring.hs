module Scoring
  ( scoreBoard,
  )
where

import Logic (isInCheck, isInCheckmate)
import Types (Board, CPiece (..), Color (..), Piece (..), swapColor)

scoreBoard :: Color -> Board -> Int
scoreBoard color board = scoreBoardInner color board - scoreBoardInner (swapColor color) board

scoreBoardInner :: Color -> Board -> Int
scoreBoardInner color board =
  (sum . map (sum . map (scoreColorPiece color))) board
    + if isInCheck color board
      then -100
      else
        0
          + if isInCheckmate color board then -1000 else 0

scoreColorPiece :: Color -> CPiece -> Int
scoreColorPiece color p@(CP pcolor _) = if color == pcolor then scorePiece p else 0
scoreColorPiece _ Null = 0

scorePiece :: CPiece -> Int
scorePiece (CP _ King) = 0
scorePiece (CP _ Queen) = 9
scorePiece (CP _ Rook) = 5
scorePiece (CP _ Bishop) = 3
scorePiece (CP _ Knight) = 3
scorePiece (CP _ Pawn) = 1
scorePiece Null = 0
