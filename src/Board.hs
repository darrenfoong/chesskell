module Board
  ( mkBoard,
    mkCoords,
    printBoard,
    prettyPrintPiece,
    readBoard,
    writeBoard,
    getPiece,
    movePiece,
    advanceBoard,
    getPositions,
    getKingPosition,
    genPossibleMoves,
    isPositionUnderAttack,
    promotePawns,
  )
where

import Data.Text (Text, chunksOf, unpack)
import Move (isValidMovePiece)
import Position (mkPositions)
import Types (Board, CMove (..), CPiece (..), CastlingSide (..), Color (..), Move, Piece (..), Position, swapColor)

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
  [ CP color (Rook False),
    CP color Knight,
    CP color Bishop,
    CP color Queen,
    CP color (King False),
    CP color Bishop,
    CP color Knight,
    CP color (Rook False)
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
printPiece (CP Black (King False)) = "K"
printPiece (CP Black (King True)) = "L"
printPiece (CP Black Queen) = "Q"
printPiece (CP Black (Rook False)) = "R"
printPiece (CP Black (Rook True)) = "S"
printPiece (CP Black Bishop) = "B"
printPiece (CP Black Knight) = "N"
printPiece (CP Black Pawn) = "P"
printPiece (CP White (King False)) = "k"
printPiece (CP White (King True)) = "l"
printPiece (CP White Queen) = "q"
printPiece (CP White (Rook False)) = "r"
printPiece (CP White (Rook True)) = "s"
printPiece (CP White Bishop) = "b"
printPiece (CP White Knight) = "n"
printPiece (CP White Pawn) = "p"
printPiece Null = "#"

unprintPiece :: String -> CPiece
unprintPiece "K" = CP Black (King False)
unprintPiece "L" = CP Black (King True)
unprintPiece "Q" = CP Black Queen
unprintPiece "R" = CP Black (Rook False)
unprintPiece "S" = CP Black (Rook True)
unprintPiece "B" = CP Black Bishop
unprintPiece "N" = CP Black Knight
unprintPiece "P" = CP Black Pawn
unprintPiece "k" = CP White (King False)
unprintPiece "l" = CP White (King True)
unprintPiece "q" = CP White Queen
unprintPiece "r" = CP White (Rook False)
unprintPiece "s" = CP White (Rook True)
unprintPiece "b" = CP White Bishop
unprintPiece "n" = CP White Knight
unprintPiece "p" = CP White Pawn
unprintPiece _ = Null

prettyPrintPiece :: CPiece -> String
prettyPrintPiece (CP Black (King _)) = "♚"
prettyPrintPiece (CP Black Queen) = "♛"
prettyPrintPiece (CP Black (Rook _)) = "♜"
prettyPrintPiece (CP Black Bishop) = "♝"
prettyPrintPiece (CP Black Knight) = "♞"
prettyPrintPiece (CP Black Pawn) = "♟︎"
prettyPrintPiece (CP White (King _)) = "♔"
prettyPrintPiece (CP White Queen) = "♕"
prettyPrintPiece (CP White (Rook _)) = "♖"
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

isValidMove :: Board -> Color -> Move -> Bool
isValidMove board color (start, end) =
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
checkLineOfSight _ (CP _ (King _)) _ = True
checkLineOfSight board (CP color Queen) move =
  checkLineOfSight board (CP color (Rook False)) move
    || checkLineOfSight board (CP color Bishop) move
checkLineOfSight board (CP _ (Rook _)) (start, end) = checkLineOfSightPos board (mkPositions start end)
checkLineOfSight board (CP _ Bishop) (start, end) = checkLineOfSightPos board (mkPositions start end)
checkLineOfSight _ (CP _ Knight) _ = True
checkLineOfSight board (CP _ Pawn) ((sc, sr), (_, er))
  | (er - sr) == 2 = checkLineOfSightPos board [(sc, sr + 1)]
  | (er - sr) == -2 = checkLineOfSightPos board [(sc, sr -1)]
  | otherwise = True

checkLineOfSightPos :: Board -> [Position] -> Bool
checkLineOfSightPos board = all (\p -> getPiece board p == Null)

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

movePiece :: Board -> CMove -> Board
movePiece board (Normal (start, end)) =
  let (intermediateBoard, oldPiece) = removePiece board start
   in setPiece intermediateBoard end oldPiece
movePiece board (Castling color side) =
  let row = case color of
        Black -> 8
        White -> 1
      (rookColumn, newRookColumn, newKingColumn) = case side of
        Short -> (8, 6, 7)
        Long -> (1, 4, 3)
      (b1, _) = removePiece board (5, row)
      (b2, _) = removePiece b1 (rookColumn, row)
      b3 = setPiece b2 (newRookColumn, row) (CP color (Rook True))
      b4 = setPiece b3 (newKingColumn, row) (CP color (King True))
   in b4

advanceBoard :: Board -> Color -> Move -> Either String Board
advanceBoard board color (start, end) =
  let cmove = case getPiece board start of
        CP _ (King _) -> case (start, end) of
          ((5, 8), (7, 8)) -> Castling Black Short
          ((5, 8), (3, 8)) -> Castling Black Long
          ((5, 1), (7, 1)) -> Castling White Short
          ((5, 1), (3, 1)) -> Castling White Long
          _ -> Normal (start, end)
        _ -> Normal (start, end)
   in case cmove of
        Normal move ->
          if isValidMove board color move
            then Right $ movePiece board cmove
            else Left $ "ERROR: Invalid move: " ++ show move
        Castling ccolor side ->
          let row = case color of
                Black -> 8
                White -> 1
              rookColumn = case side of
                Short -> 8
                Long -> 1
              intermediateColumns = case side of
                Short -> [6, 7]
                Long -> [2, 3, 4]
              king = getPiece board (5, row)
              rook = getPiece board (rookColumn, row)
           in if color == ccolor
                && king == CP color (King False)
                && rook == CP color (Rook False)
                && all (\c -> getPiece board (c, row) == Null) intermediateColumns
                then Right $ movePiece board cmove
                else Left $ "ERROR: Invalid castling move: " ++ show color ++ " " ++ show side

getPositions :: Board -> Color -> [Position]
getPositions board color =
  foldl
    ( \ps p -> case getPiece board p of
        CP clr _ -> if color == clr then p : ps else ps
        _ -> ps
    )
    []
    mkCoords

getKingPosition :: Board -> Color -> Maybe Position
getKingPosition board color =
  let isKing p = case getPiece board p of
        CP _ (King _) -> True
        _ -> False
   in case filter isKing (getPositions board color) of
        [] -> Nothing
        p : _ -> Just p

genPossibleMovesPiece :: Board -> Position -> [Move]
genPossibleMovesPiece board position = case getPiece board position of
  CP color _ -> filter (isValidMove board color) (map (\pos -> (position, pos)) mkCoords)
  _ -> []

genPossibleMoves :: Board -> Color -> [Move]
genPossibleMoves board color = concatMap (genPossibleMovesPiece board) (getPositions board color)

isPositionUnderAttack :: Board -> Color -> Position -> Bool
isPositionUnderAttack board color pos = elem pos $ map snd $ genPossibleMoves board $ swapColor color

promotePawn :: [CPiece] -> [CPiece]
promotePawn [] = []
promotePawn (CP color Pawn : ps) = CP color Queen : promotePawn ps
promotePawn (p : ps) = p : promotePawn ps

promotePawns :: Board -> Board
promotePawns board =
  let firstRow = head board
      lastRow = board !! 7
   in promotePawn firstRow : take 6 (tail board) ++ [promotePawn lastRow]
