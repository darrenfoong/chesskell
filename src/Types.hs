module Types
(
  Piece(..),
  Color(..),
  CPiece(..),
  Board,
  Position,
  Move
) where

data Piece  = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq
data Color  = Black | White deriving (Eq, Show)
data CPiece = CP Color Piece | Null

type Board    = [[CPiece]]
type Position = (Int, Int)
type Move     = (Position, Position)
