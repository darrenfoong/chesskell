{-# OPTIONS -Wno-unused-top-binds #-}

module Scoring
  ( scoreBoard,
    scoreBoardTableInner,
  )
where

import Board (getPiece, mkCoords)
import Logic (isInCheck, isInCheckmate)
import Types (Board, CPiece (..), Color (..), Piece (..), Position, swapColor)

scoreBoard :: Board -> Color -> Int
scoreBoard board color = scoreBoardTableInner board color - scoreBoardTableInner board (swapColor color)

scoreBoardInner :: Board -> Color -> Int
scoreBoardInner board color =
  (sum . map (sum . map (scoreColorPiece color))) board
    + if isInCheck board color
      then -100
      else
        0
          + if isInCheckmate board color then -1000 else 0

scoreColorPiece :: Color -> CPiece -> Int
scoreColorPiece color p@(CP pcolor _) = if color == pcolor then scorePiece p else 0
scoreColorPiece _ Null = 0

scorePiece :: CPiece -> Int
scorePiece (CP _ (King _)) = 0
scorePiece (CP _ Queen) = 9
scorePiece (CP _ (Rook _)) = 5
scorePiece (CP _ Bishop) = 3
scorePiece (CP _ Knight) = 3
scorePiece (CP _ Pawn) = 1
scorePiece Null = 0

scoreBoardTableInner :: Board -> Color -> Int
scoreBoardTableInner board color =
  sum (map (scoreBoardTablePosition board color) mkCoords)
    + if isInCheck board color
      then -1000
      else
        0
          + if isInCheckmate board color then -10000 else 0

scoreBoardTablePosition :: Board -> Color -> Position -> Int
scoreBoardTablePosition board color position =
  case getPiece board position of
    CP pcolor p ->
      if color == pcolor
        then
          let table = getTable color p
           in getTableValue table position
        else 0
    Null -> 0

getTableValue :: [[Int]] -> Position -> Int
getTableValue table (cn, rn) = table !! (rn -1) !! (cn -1)

getTable :: Color -> Piece -> [[Int]]
getTable White = reverse . getBlackTable
getTable Black = getBlackTable

-- obtained from https://www.chessprogramming.org/Simplified_Evaluation_Function

getBlackTable :: Piece -> [[Int]]
getBlackTable (King _) =
  [ [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-20, -30, -30, -40, -40, -30, -30, -20],
    [-10, -20, -20, -20, -20, -20, -20, -10],
    [20, 20, 0, 0, 0, 0, 20, 20],
    [20, 30, 10, 0, 0, 10, 30, 20]
  ]
getBlackTable Queen =
  [ [-20, -10, -10, -5, -5, -10, -10, -20],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 5, 5, 5, 0, -10],
    [-5, 0, 5, 5, 5, 5, 0, -5],
    [0, 0, 5, 5, 5, 5, 0, -5],
    [-10, 5, 5, 5, 5, 5, 0, -10],
    [-10, 0, 5, 0, 0, 0, 0, -10],
    [-20, -10, -10, -5, -5, -10, -10, -20]
  ]
getBlackTable (Rook _) =
  [ [0, 0, 0, 0, 0, 0, 0, 0],
    [5, 10, 10, 10, 10, 10, 10, 5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [0, 0, 0, 5, 5, 0, 0, 0]
  ]
getBlackTable Bishop =
  [ [-20, -10, -10, -10, -10, -10, -10, -20],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 10, 10, 5, 0, -10],
    [-10, 5, 5, 10, 10, 5, 5, -10],
    [-10, 0, 10, 10, 10, 10, 0, -10],
    [-10, 10, 10, 10, 10, 10, 10, -10],
    [-10, 5, 0, 0, 0, 0, 5, -10],
    [-20, -10, -10, -10, -10, -10, -10, -20]
  ]
getBlackTable Knight =
  [ [-50, -40, -30, -30, -30, -30, -40, -50],
    [-40, -20, 0, 0, 0, 0, -20, -40],
    [-30, 0, 10, 15, 15, 10, 0, -30],
    [-30, 5, 15, 20, 20, 15, 5, -30],
    [-30, 0, 15, 20, 20, 15, 0, -30],
    [-30, 5, 10, 15, 15, 10, 5, -30],
    [-40, -20, 0, 5, 5, 0, -20, -40],
    [-50, -40, -30, -30, -30, -30, -40, -50]
  ]
getBlackTable Pawn =
  [ [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 50, 50, 50, 50, 50, 50, 50],
    [0, 10, 20, 30, 30, 20, 10, 10],
    [5, 5, 10, 25, 25, 10, 5, 5],
    [0, 0, 0, 20, 20, 0, 0, 0],
    [5, -5, -10, 0, 0, -10, -5, 5],
    [5, 10, 10, -20, -20, 10, 10, 5],
    [0, 0, 0, 0, 0, 0, 0, 0]
  ]
