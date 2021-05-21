module Logic
  ( scoreBoard,
    genMove,
    isInCheckmate,
    isInCheck,
    promotePawns,
    respondBoard,
  )
where

import Board (advanceBoard, getPiece, mkCoords, movePiece, validMove)
import Data.Either (fromRight)
import System.Random
import System.Random.Shuffle (shuffle')
import Types (Board, CPiece (..), Color (..), Move, Piece (..), Position, swapColor)

posInfinity :: Int
posInfinity = 100000

negInfinity :: Int
negInfinity = - posInfinity

scoreBoard :: Color -> Board -> Int
scoreBoard color board = scoreBoardInner color board - scoreBoardInner (swapColor color) board

scoreBoardInner :: Color -> Board -> Int
scoreBoardInner color board =
  (sum . map (sum . map (scoreColorPiece color))) board
    + 2 * scoreBoardCenter color board
    + if isInCheck color board
      then -100
      else
        0
          + if isInCheckmate color board then -1000 else 0

scoreBoardCenter :: Color -> Board -> Int
scoreBoardCenter color board =
  let centerPieces = [getPiece board (c, r) | c <- [3 .. 6], r <- [3 .. 6]]
   in (sum . map (scoreColorPiece color)) centerPieces

scoreColorPiece :: Color -> CPiece -> Int
scoreColorPiece color p@(CP pcolor _) = if color == pcolor then scorePiece p else 0
scoreColorPiece _ Null = 0

scorePiece :: CPiece -> Int
scorePiece (CP _ King) = 0
scorePiece (CP _ Queen) = 9
scorePiece (CP _ Rook) = 5
scorePiece (CP _ Bishop) = 3
scorePiece (CP _ Knight) = 3
scorePiece (CP _ Pawn) = 1
scorePiece Null = 0

isInCheckmate :: Color -> Board -> Bool
isInCheckmate color board = all (isInCheck color) ((:) board $ map snd $ genNextMoveBoards board color)

isInCheck :: Color -> Board -> Bool
isInCheck color board = maybe False (isUnderAttack color board) (getKingPosition color board)

isUnderAttack :: Color -> Board -> Position -> Bool
isUnderAttack color board pos = elem pos $ map snd $ genMoves board $ swapColor color

getKingPosition :: Color -> Board -> Maybe Position
getKingPosition color board = case filter (\p -> getPiece board p == CP color King) (genPositions board color) of
  [] -> Nothing
  p : _ -> Just p

promotePawn :: [CPiece] -> [CPiece]
promotePawn [] = []
promotePawn (CP color Pawn : ps) = CP color Queen : promotePawn ps
promotePawn (p : ps) = p : promotePawn ps

promotePawns :: Board -> Board
promotePawns board =
  let firstRow = head board
      lastRow = board !! 7
   in promotePawn firstRow : take 6 (tail board) ++ [promotePawn lastRow]

respondBoard :: StdGen -> Board -> Color -> (StdGen, Either String Board)
respondBoard gen board color =
  let (newGen, mMove) = genMove gen board color
   in case mMove of
        Just m -> (newGen, advanceBoard board m color)
        Nothing -> (newGen, Left "ERROR: Program has made an invalid move")

genMove :: StdGen -> Board -> Color -> (StdGen, Maybe Move)
genMove gen board color =
  let (gen1, gen2) = split gen
      f [] = []
      f ms = shuffle' ms (length ms) gen1
   in case minimax f board color color 3 negInfinity posInfinity True of
        (_, _, Nothing) -> (gen2, Nothing)
        (_, _, Just (_, m)) -> (gen2, Just m)

minimax :: ([Move] -> [Move]) -> Board -> Color -> Color -> Int -> Int -> Int -> Bool -> (Int, Int, Maybe (Int, Move))
minimax f board scoringColor playerColor n alpha beta maximising =
  let g a b mPreviousBestMove previousBestScore [] = case mPreviousBestMove of
        Nothing -> (a, b, Nothing)
        Just previousBestMove -> (a, b, Just (previousBestScore, previousBestMove))
      g a b Nothing _ [m] =
        let s = scoreBoard scoringColor $ movePiece board m
         in (a, b, Just (s, m))
      g a b mPreviousBestMove previousBestScore (m : ms) =
        if n == 1
          then
            let s = scoreBoard scoringColor $ movePiece board m
             in if maximising
                  then
                    let currentBestScore = max previousBestScore s
                        updatedA = max a currentBestScore
                     in if updatedA >= b
                          then (updatedA, b, Just (currentBestScore, m))
                          else g updatedA b mPreviousBestMove previousBestScore ms
                  else
                    let currentBestScore = min previousBestScore s
                        updatedB = min b currentBestScore
                     in if updatedB <= a
                          then (a, updatedB, Just (currentBestScore, m))
                          else g a updatedB mPreviousBestMove previousBestScore ms
          else do
            case minimax f (movePiece board m) scoringColor (swapColor playerColor) (n -1) a b (not maximising) of
              (newA, newB, Nothing) -> g newA newB mPreviousBestMove previousBestScore ms
              (newA, newB, Just (s, _)) ->
                if maximising
                  then
                    let currentBestScore = max previousBestScore s
                        updatedA = max newA currentBestScore
                     in if updatedA >= newB
                          then (updatedA, newB, Just (currentBestScore, m))
                          else g updatedA newB mPreviousBestMove previousBestScore ms
                  else
                    let currentBestScore = min previousBestScore s
                        updatedB = min newB currentBestScore
                     in if updatedB <= newA
                          then (newA, updatedB, Just (currentBestScore, m))
                          else g newA updatedB mPreviousBestMove previousBestScore ms
      initialBestScore = if maximising then negInfinity else posInfinity
   in g alpha beta Nothing initialBestScore $ f $ genNonCheckMoves board playerColor

genNextMoveBoards :: Board -> Color -> [(Move, Board)]
genNextMoveBoards board color =
  let possibleMoves = genMoves board color
      possibleMoveBoards = map (\m -> (m, fromRight [] $ advanceBoard board m color)) possibleMoves
   in filter (\(_, b) -> b /= []) possibleMoveBoards

genNonCheckMoves :: Board -> Color -> [Move]
genNonCheckMoves board color = map fst $ filter (\(_, b) -> not $ isInCheck color b) $ genNextMoveBoards board color

genMoves :: Board -> Color -> [Move]
genMoves board color = concatMap (genPossibleMovesPiece board) (genPositions board color)

genPositions :: Board -> Color -> [Position]
genPositions board color =
  foldl
    ( \ps p -> case getPiece board p of
        CP clr _ -> if color == clr then p : ps else ps
        _ -> ps
    )
    []
    mkCoords

genPossibleMovesPiece :: Board -> Position -> [Move]
genPossibleMovesPiece board position = case getPiece board position of
  CP color _ -> filter (\move -> validMove board move color) (map (\pos -> (position, pos)) mkCoords)
  _ -> []
