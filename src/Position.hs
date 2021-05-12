module Position
  ( mkPosition,
    mkPositions,
    mkPositionsInner,
    isValidPosition,
  )
where

import Data.Char
import Types (Position)

mkPosition :: (Char, Char) -> Position
mkPosition (c, r) = (ord c - ord 'a' + 1, digitToInt r)

mkPositions :: Position -> Position -> [Position]
mkPositions (sc, sr) (ec, er) =
  let cdelta = compareToInt sc ec
      rdelta = compareToInt sr er
      ps = mkPositionsInner (sc, sr) (ec, er) cdelta rdelta
   in if length ps <= 1 then [] else tail ps

mkPositionsInner :: Position -> Position -> Int -> Int -> [Position]
mkPositionsInner (sc, sr) (ec, er) cdelta rdelta
  | sc == ec && sr == er = []
  | otherwise = (sc, sr) : mkPositionsInner (sc + cdelta, sr + rdelta) (ec, er) cdelta rdelta

isValidPosition :: Position -> Bool
isValidPosition (c, r) = (1 <= c) && (c <= 8) && (1 <= r) && (r <= 8)

compareToInt :: Int -> Int -> Int
compareToInt a b
  | a > b = -1
  | a < b = 1
  | otherwise = 0
