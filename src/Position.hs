module Position
(
  mkPos,
  validPos
) where

import Data.Char

import Types (Position)

mkPos :: (Char, Char) -> Position
mkPos (c,r) = (ord c - ord 'a' + 1, digitToInt r)

validPos :: Position -> Bool
validPos (c,r) = (1 <= c) && (c <= 8) && (1 <= r) && (r <= 8)
