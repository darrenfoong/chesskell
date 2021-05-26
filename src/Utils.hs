module Utils (shuffle) where

import System.Random
import System.Random.Shuffle (shuffle')

shuffle :: StdGen -> [a] -> [a]
shuffle _ [] = []
shuffle gen xs = shuffle' xs (length xs) gen
