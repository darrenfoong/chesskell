module Types
  ( Piece (..),
    Color (..),
    CPiece (..),
    Board,
    Position,
    Move,
    CastlingSide (..),
    CMove (..),
    swapColor,
  )
where

data Piece = King Bool | Queen | Rook Bool | Bishop | Knight | Pawn Bool deriving (Eq)

-- King True/False: castled/uncastled
-- Rook True/False: castled/uncastled
-- Pawn True/False: previous move was two squares from starting square/otherwise

data Color = Black | White deriving (Eq, Show)

data CPiece = CP Color Piece | Null deriving (Eq)

type Board = [[CPiece]]

type Position = (Int, Int)

type Move = (Position, Position)

data CastlingSide = Short | Long deriving (Show)

data CMove = Normal Move | Castling Color CastlingSide | EnPassant Color Move deriving (Show)

swapColor :: Color -> Color
swapColor Black = White
swapColor White = Black
