module Types
  ( Piece (..),
    Color (..),
    CPiece (..),
    Board,
    Position,
    Move,
    CastlingSide,
    CMove (..),
    swapColor,
  )
where

data Piece = King Bool | Queen | Rook Bool | Bishop | Knight | Pawn deriving (Eq)

data Color = Black | White deriving (Eq, Show)

data CPiece = CP Color Piece | Null deriving (Eq)

type Board = [[CPiece]]

type Position = (Int, Int)

type Move = (Position, Position)

data CastlingSide = Short | Long deriving (Show)

data CMove = Normal Move | Castling Color CastlingSide

swapColor :: Color -> Color
swapColor Black = White
swapColor White = Black
