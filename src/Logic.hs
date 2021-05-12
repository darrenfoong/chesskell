module Logic
  ( scoreBoard,
    genMove,
    isInCheckmate,
    isInCheck,
    respondBoard,
  )
where

import Board (advanceBoard, getPiece, mkCoords, movePiece, validMove)
import Data.Either (fromRight)
import Data.List
import System.Random
import Types (Board, CPiece (..), Color (..), Move, Piece (..), Position, swapColor)

scoreBoard :: Color -> Board -> Int
scoreBoard color board = (sum . map (sum . map (scoreColorPiece color))) board + if isInCheck color board then -1000 else 0 + 2 * scoreBoardCenter color board

scoreColorPiece :: Color -> CPiece -> Int
scoreColorPiece color p@(CP pcolor _) = if color == pcolor then scorePiece p else 0
scoreColorPiece _ Null = 0

scoreBoardCenter :: Color -> Board -> Int
scoreBoardCenter color board =
  let centerPieces = [(getPiece board) (c, r) | c <- [3 .. 6], r <- [3 .. 6]]
   in (sum . map (scoreColorPiece color)) centerPieces

scorePiece :: CPiece -> Int
scorePiece (CP _ King) = 0
scorePiece (CP _ Queen) = 9
scorePiece (CP _ Rook) = 5
scorePiece (CP _ Bishop) = 3
scorePiece (CP _ Knight) = 3
scorePiece (CP _ Pawn) = 1
scorePiece Null = 0

isInCheckmate :: Color -> Board -> Bool
isInCheckmate color board =
  let possibleMoves = genMoves board color
      possibleNextBoards = map (\m -> fromRight [] $ advanceBoard board m color) possibleMoves
      filteredPossibleNextBoards = filter (/= []) possibleNextBoards
   in all (isInCheck color) (board : filteredPossibleNextBoards)

isInCheck :: Color -> Board -> Bool
isInCheck color board = maybe False (isUnderAttack color board) (getKingPosition color board)

isUnderAttack :: Color -> Board -> Position -> Bool
isUnderAttack color board pos = elem pos $ map snd $ genMoves board $ swapColor color

getKingPosition :: Color -> Board -> Maybe Position
getKingPosition color board = case filter (\p -> getPiece board p == CP color King) (genPositions board color) of
  [] -> Nothing
  p : _ -> Just p

respondBoard :: StdGen -> Board -> Color -> (StdGen, Either String Board)
respondBoard gen board color =
  let (newGen, mMove) = genMove gen board color
   in case mMove of
        Just m -> (newGen, advanceBoard board m color)
        Nothing -> (newGen, Left "ERROR: Program has made an invalid move")

genMove :: StdGen -> Board -> Color -> (StdGen, Maybe Move)
genMove gen board color =
  let (_, m) = minimax board color color 3 True
   in (gen, Just m)

minimax :: Board -> Color -> Color -> Int -> Bool -> (Int, Move)
minimax board scoringColor playerColor 1 maximising = maximumBy (compareMove maximising) $ map (\m -> (scoreBoard scoringColor (movePiece board m), m)) $ genMoves board playerColor
minimax board scoringColor playerColor n maximising = maximumBy (compareMove maximising) $ map (\m -> let (s, _) = minimax (movePiece board m) scoringColor (swapColor playerColor) (n -1) (not maximising) in (s, m)) $ genMoves board playerColor

compareMove :: Bool -> (Int, Move) -> (Int, Move) -> Ordering
compareMove maximising (s1, _) (s2, _) = if maximising then compare s1 s2 else compare s2 s1

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
