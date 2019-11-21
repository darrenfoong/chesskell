module Position
(
  mkPos,
  mkPositions,
  validPos
) where

import Data.Char

import Types (Position)

mkPos :: (Char, Char) -> Position
mkPos (c,r) = (ord c - ord 'a' + 1, digitToInt r)

mkPositions :: Position -> Position -> [Position]
mkPositions (sc,sr) (ec,er) = let cdelta = compareToInt sc ec
                                  rdelta = compareToInt sr er in
                                [(c,r) | c <- [sc,sc+cdelta..ec], r <- [sr,sr+rdelta..er]]

validPos :: Position -> Bool
validPos (c,r) = (1 <= c) && (c <= 8) && (1 <= r) && (r <= 8)

compareToInt :: Int -> Int -> Int
compareToInt a b
  | a > b     = -1
  | a < b     = 1
  | otherwise = 0
