module Types
(
  Piece(..),
  Color(..),
  CPiece(..),
  Board,
  Position,
  Move,
  swapColor,
  compareToInt,
  mkPositions
) where

data Piece  = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq
data Color  = Black | White deriving (Eq, Show)
data CPiece = CP Color Piece | Null

type Board    = [[CPiece]]
type Position = (Int, Int)
type Move     = (Position, Position)

swapColor :: Color -> Color
swapColor Black = White
swapColor White = Black

compareToInt :: Int -> Int -> Int
compareToInt a b
  | a > b     = -1
  | a < b     = 1
  | otherwise = 0

mkPositions :: Position -> Position -> [Position]
mkPositions (sc,sr) (ec,er) = let cdelta = compareToInt sc ec
                                  rdelta = compareToInt sr er in
                                [(c,r) | c <- [sc,sc+cdelta..ec], r <- [sr,sr+rdelta..er]]
