module Board
  ( mkBoard,
    printBoard,
    prettyPrintPiece,
    readBoard,
    writeBoard,
    advanceBoard,
    movePiece,
    getPiece,
    getPositions,
    getKingPosition,
    promotePawns,
    isPositionUnderAttack,
    genPossibleMoves,
  )
where

import Data.Text (Text, chunksOf, unpack)
import Move (isValidMovePiece)
import Position (mkPositions)
import Types (Board, CPiece (..), Color (..), Move, Piece (..), Position, swapColor)

mkBoard :: Board
mkBoard =
  [ mkMixedRow White,
    mkPawnRow White,
    mkBlankRow,
    mkBlankRow,
    mkBlankRow,
    mkBlankRow,
    mkPawnRow Black,
    mkMixedRow Black
  ]

mkMixedRow :: Color -> [CPiece]
mkMixedRow color =
  [ CP color Rook,
    CP color Knight,
    CP color Bishop,
    CP color Queen,
    CP color King,
    CP color Bishop,
    CP color Knight,
    CP color Rook
  ]

mkPawnRow :: Color -> [CPiece]
mkPawnRow color = replicate 8 (CP color Pawn)

mkBlankRow :: [CPiece]
mkBlankRow = replicate 8 Null

mkCoords :: [Position]
mkCoords = [(c, r) | c <- [1 .. 8], r <- [1 .. 8]]

printBoard :: Board -> Bool -> String
printBoard board debug =
  let result = map (\(r, n) -> show n ++ " " ++ printRow r) $ f $ zip board ([1 .. 8] :: [Int])
   in unlines $ result ++ extraLines
  where
    f = if debug then id else reverse
    extraLines = if debug then ["  12345678", "  abcdefgh"] else ["  abcdefgh"]

printRow :: [CPiece] -> String
printRow = concatMap printPiece

printPiece :: CPiece -> String
printPiece (CP Black King) = "K"
printPiece (CP Black Queen) = "Q"
printPiece (CP Black Rook) = "R"
printPiece (CP Black Bishop) = "B"
printPiece (CP Black Knight) = "N"
printPiece (CP Black Pawn) = "P"
printPiece (CP White King) = "k"
printPiece (CP White Queen) = "q"
printPiece (CP White Rook) = "r"
printPiece (CP White Bishop) = "b"
printPiece (CP White Knight) = "n"
printPiece (CP White Pawn) = "p"
printPiece Null = "#"

unprintPiece :: String -> CPiece
unprintPiece "K" = CP Black King
unprintPiece "Q" = CP Black Queen
unprintPiece "R" = CP Black Rook
unprintPiece "B" = CP Black Bishop
unprintPiece "N" = CP Black Knight
unprintPiece "P" = CP Black Pawn
unprintPiece "k" = CP White King
unprintPiece "q" = CP White Queen
unprintPiece "r" = CP White Rook
unprintPiece "b" = CP White Bishop
unprintPiece "n" = CP White Knight
unprintPiece "p" = CP White Pawn
unprintPiece _ = Null

prettyPrintPiece :: CPiece -> String
prettyPrintPiece (CP Black King) = "♚"
prettyPrintPiece (CP Black Queen) = "♛"
prettyPrintPiece (CP Black Rook) = "♜"
prettyPrintPiece (CP Black Bishop) = "♝"
prettyPrintPiece (CP Black Knight) = "♞"
prettyPrintPiece (CP Black Pawn) = "♟︎"
prettyPrintPiece (CP White King) = "♔"
prettyPrintPiece (CP White Queen) = "♕"
prettyPrintPiece (CP White Rook) = "♖"
prettyPrintPiece (CP White Bishop) = "♗"
prettyPrintPiece (CP White Knight) = "♘"
prettyPrintPiece (CP White Pawn) = "♙"
prettyPrintPiece Null = ""

readBoard :: Text -> Either String Board
readBoard boardStr = Right (map readRow $ chunksOf 8 boardStr)

readRow :: Text -> [CPiece]
readRow rowStr = map (unprintPiece . unpack) $ chunksOf 1 rowStr

writeBoard :: Board -> String
writeBoard = concatMap printRow

promotePawn :: [CPiece] -> [CPiece]
promotePawn [] = []
promotePawn (CP color Pawn : ps) = CP color Queen : promotePawn ps
promotePawn (p : ps) = p : promotePawn ps

promotePawns :: Board -> Board
promotePawns board =
  let firstRow = head board
      lastRow = board !! 7
   in promotePawn firstRow : take 6 (tail board) ++ [promotePawn lastRow]

isValidMove :: Board -> Move -> Color -> Bool
isValidMove board (start, end) color =
  start /= end
    && let startPiece = getPiece board start
        in case startPiece of
             CP startColor _ ->
               startColor == color
                 && case getPiece board end of
                   Null ->
                     isValidMovePiece startPiece False (start, end)
                       && checkLineOfSight board startPiece (start, end)
                   CP endColor _ ->
                     startColor /= endColor
                       && isValidMovePiece startPiece True (start, end)
                       && checkLineOfSight board startPiece (start, end)
             Null -> False

checkLineOfSight :: Board -> CPiece -> Move -> Bool
checkLineOfSight _ Null _ = False
checkLineOfSight _ (CP _ King) _ = True
checkLineOfSight board (CP color Queen) move =
  checkLineOfSight board (CP color Rook) move
    || checkLineOfSight board (CP color Bishop) move
checkLineOfSight board (CP _ Rook) (start, end) = checkLineOfSightPos board (mkPositions start end)
checkLineOfSight board (CP _ Bishop) (start, end) = checkLineOfSightPos board (mkPositions start end)
checkLineOfSight _ (CP _ Knight) _ = True
checkLineOfSight board (CP _ Pawn) ((sc, sr), (_, er))
  | (er - sr) == 2 = checkLineOfSightPos board [(sc, sr + 1)]
  | (er - sr) == -2 = checkLineOfSightPos board [(sc, sr -1)]
  | otherwise = True

movePiece :: Board -> Move -> Board
movePiece board (start, end) =
  let (intermediateBoard, oldPiece) = removePiece board start
   in setPiece intermediateBoard end oldPiece

alterBoardRow :: Board -> Int -> ([CPiece] -> ([CPiece], CPiece)) -> (Board, CPiece)
alterBoardRow [] _ _ = ([], Null)
alterBoardRow (r : rs) 1 f =
  let (alteredRow, oldPiece) = f r
   in (alteredRow : rs, oldPiece)
alterBoardRow (r : rs) n f =
  let (alteredBoard, oldPiece) = alterBoardRow rs (n -1) f
   in (r : alteredBoard, oldPiece)

alterRow :: [CPiece] -> Int -> (CPiece -> CPiece) -> ([CPiece], CPiece)
alterRow [] _ _ = ([], Null)
alterRow (p : ps) 1 f = (f p : ps, p)
alterRow (p : ps) n f =
  let (alteredRow, oldPiece) = alterRow ps (n -1) f
   in (p : alteredRow, oldPiece)

getPiece :: Board -> Position -> CPiece
getPiece board (cn, rn) = board !! (rn -1) !! (cn -1)

setPiece :: Board -> Position -> CPiece -> Board
setPiece board (cn, rn) piece =
  let (alteredBoard, _) = alterBoardRow board rn (\r -> alterRow r cn (const piece))
   in alteredBoard

removePiece :: Board -> Position -> (Board, CPiece)
removePiece board (cn, rn) = alterBoardRow board rn (\r -> alterRow r cn (const Null))

checkLineOfSightPos :: Board -> [Position] -> Bool
checkLineOfSightPos board = all (\p -> getPiece board p == Null)

advanceBoard :: Board -> Move -> Color -> Either String Board
advanceBoard board move color =
  if isValidMove board move color
    then Right $ movePiece board move
    else Left $ "ERROR: Invalid move: " ++ show move

getPositions :: Board -> Color -> [Position]
getPositions board color =
  foldl
    ( \ps p -> case getPiece board p of
        CP clr _ -> if color == clr then p : ps else ps
        _ -> ps
    )
    []
    mkCoords

getKingPosition :: Color -> Board -> Maybe Position
getKingPosition color board = case filter (\p -> getPiece board p == CP color King) (getPositions board color) of
  [] -> Nothing
  p : _ -> Just p

genPossibleMovesPiece :: Board -> Position -> [Move]
genPossibleMovesPiece board position = case getPiece board position of
  CP color _ -> filter (\move -> isValidMove board move color) (map (\pos -> (position, pos)) mkCoords)
  _ -> []

genPossibleMoves :: Board -> Color -> [Move]
genPossibleMoves board color = concatMap (genPossibleMovesPiece board) (getPositions board color)

isPositionUnderAttack :: Color -> Board -> Position -> Bool
isPositionUnderAttack color board pos = elem pos $ map snd $ genPossibleMoves board $ swapColor color
