module Board
(
  mkBoard,
  mkPos,
  mkCoords,
  printBoard,
  scoreBoard
) where

import Data.Char

import Types (Piece(..), Color(..), CPiece(..), Board, Position, Move)

mkBoard :: Board
mkBoard = [mkMixedRow White,
           mkPawnRow White,
           mkBlankRow,
           mkBlankRow,
           mkBlankRow,
           mkBlankRow,
           mkPawnRow Black,
           mkMixedRow Black]

mkMixedRow :: Color -> [CPiece]
mkMixedRow color = [CP color Rook,
                    CP color Knight,
                    CP color Bishop,
                    CP color Queen,
                    CP color King,
                    CP color Bishop,
                    CP color Knight,
                    CP color Rook]

mkPawnRow :: Color -> [CPiece]
mkPawnRow color = replicate 8 (CP color Pawn)

mkBlankRow :: [CPiece]
mkBlankRow = replicate 8 Null

mkPos :: (Char, Char) -> Position
mkPos (c,r) = (ord c - ord 'a' + 1, digitToInt r)

mkCoords :: [Position]
mkCoords = [(c,r) | c <- [1..8], r <- [1..8]]

printBoard :: Board -> Bool -> String
printBoard board debug = let result = map (\(r,n) -> show n ++ " " ++ printRow r) $ f $ zip board ([1..8] :: [Int]) in
                           unlines $ result ++ extraLines
                         where f = if debug then id else reverse
                               extraLines = if debug then ["  12345678", "  abcdefgh"] else ["  abcdefgh"]

printRow :: [CPiece] -> String
printRow = concatMap printPiece

printPiece :: CPiece -> String
printPiece (CP Black King)   = "K"
printPiece (CP Black Queen)  = "Q"
printPiece (CP Black Rook)   = "R"
printPiece (CP Black Bishop) = "B"
printPiece (CP Black Knight) = "N"
printPiece (CP Black Pawn)   = "P"
printPiece (CP White King)   = "k"
printPiece (CP White Queen)  = "q"
printPiece (CP White Rook)   = "r"
printPiece (CP White Bishop) = "b"
printPiece (CP White Knight) = "n"
printPiece (CP White Pawn)   = "p"
printPiece Null              = "#"

scoreBoard :: Color -> Board -> Int
scoreBoard color = sum . map (sum . map (scoreColorPiece color))

scoreColorPiece :: Color -> CPiece -> Int
scoreColorPiece color p@(CP pcolor _) = if color == pcolor then scorePiece p else 0
scoreColorPiece _ Null = 0

scorePiece :: CPiece -> Int
scorePiece (CP _ King)   = 0
scorePiece (CP _ Queen)  = 9
scorePiece (CP _ Rook)   = 5
scorePiece (CP _ Bishop) = 3
scorePiece (CP _ Knight) = 3
scorePiece (CP _ Pawn)   = 1
scorePiece Null          = 0
