module Minimax
  ( posInfinity,
    negInfinity,
    minimax,
  )
where

import Board (movePiece)
import Types (Board, Color (..), Move, swapColor)

posInfinity :: Int
posInfinity = 100000

negInfinity :: Int
negInfinity = - posInfinity

compareScoreMove :: (Int -> Int -> Bool) -> (Int, Maybe Move) -> (Int, Maybe Move) -> (Int, Maybe Move)
compareScoreMove f (s1, m1) (s2, m2) = if f s1 s2 then (s1, m1) else (s2, m2)

minimax :: (Board -> Color -> Int) -> (Board -> Color -> [Move]) -> ([Move] -> [Move]) -> Board -> Color -> Color -> Int -> Int -> Int -> Bool -> Maybe (Int, Move)
minimax boardScorer nextStateGen movesTransformer board scoringColor playerColor n alpha beta maximising =
  let extractMove previousBestScore mPreviousBestMove = do
        previousBestMove <- mPreviousBestMove
        Just (previousBestScore, previousBestMove)
      updateAlphaBeta a b maximising' s m previousBestScore mPreviousBestMove ms =
        if maximising'
          then
            let (currentBestScore, mCurrentBestMove) = compareScoreMove (>=) (s, Just m) (previousBestScore, mPreviousBestMove)
                updatedA = max a currentBestScore
             in if updatedA >= b
                  then extractMove currentBestScore mCurrentBestMove
                  else minimaxInner updatedA b currentBestScore mCurrentBestMove ms
          else
            let (currentBestScore, mCurrentBestMove) = compareScoreMove (<=) (s, Just m) (previousBestScore, mPreviousBestMove)
                updatedB = min b currentBestScore
             in if updatedB <= a
                  then extractMove currentBestScore mCurrentBestMove
                  else minimaxInner a updatedB currentBestScore mCurrentBestMove ms
      minimaxInner _ _ previousBestScore mPreviousBestMove [] = extractMove previousBestScore mPreviousBestMove
      minimaxInner _ _ _ Nothing [m] = Just (boardScorer (movePiece board m) scoringColor, m)
      minimaxInner a b previousBestScore mPreviousBestMove (m : ms) =
        if n == 1
          then
            let s = boardScorer (movePiece board m) scoringColor
             in updateAlphaBeta a b maximising s m previousBestScore mPreviousBestMove ms
          else do
            case minimax boardScorer nextStateGen movesTransformer (movePiece board m) scoringColor (swapColor playerColor) (n -1) a b (not maximising) of
              Nothing -> minimaxInner a b previousBestScore mPreviousBestMove ms
              Just (s, _) -> updateAlphaBeta a b maximising s m previousBestScore mPreviousBestMove ms
      initialBestScore = if maximising then negInfinity else posInfinity
   in minimaxInner alpha beta initialBestScore Nothing $ movesTransformer $ nextStateGen board playerColor
