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
    resetPawns,
  )
where

import Data.Maybe (fromMaybe)
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
mkPawnRow color = replicate 8 (CP color (Pawn False))

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
printPiece (CP Black (Pawn False)) = "P"
printPiece (CP Black (Pawn True)) = "O"
printPiece (CP White (King False)) = "k"
printPiece (CP White (King True)) = "l"
printPiece (CP White Queen) = "q"
printPiece (CP White (Rook False)) = "r"
printPiece (CP White (Rook True)) = "s"
printPiece (CP White Bishop) = "b"
printPiece (CP White Knight) = "n"
printPiece (CP White (Pawn False)) = "p"
printPiece (CP White (Pawn True)) = "o"
printPiece Null = "#"

unprintPiece :: String -> CPiece
unprintPiece "K" = CP Black (King False)
unprintPiece "L" = CP Black (King True)
unprintPiece "Q" = CP Black Queen
unprintPiece "R" = CP Black (Rook False)
unprintPiece "S" = CP Black (Rook True)
unprintPiece "B" = CP Black Bishop
unprintPiece "N" = CP Black Knight
unprintPiece "P" = CP Black (Pawn False)
unprintPiece "O" = CP Black (Pawn True)
unprintPiece "k" = CP White (King False)
unprintPiece "l" = CP White (King True)
unprintPiece "q" = CP White Queen
unprintPiece "r" = CP White (Rook False)
unprintPiece "s" = CP White (Rook True)
unprintPiece "b" = CP White Bishop
unprintPiece "n" = CP White Knight
unprintPiece "p" = CP White (Pawn False)
unprintPiece "o" = CP White (Pawn True)
unprintPiece _ = Null

prettyPrintPiece :: CPiece -> String
prettyPrintPiece (CP Black (King _)) = "♚"
prettyPrintPiece (CP Black Queen) = "♛"
prettyPrintPiece (CP Black (Rook _)) = "♜"
prettyPrintPiece (CP Black Bishop) = "♝"
prettyPrintPiece (CP Black Knight) = "♞"
prettyPrintPiece (CP Black (Pawn _)) = "♟︎"
prettyPrintPiece (CP White (King _)) = "♔"
prettyPrintPiece (CP White Queen) = "♕"
prettyPrintPiece (CP White (Rook _)) = "♖"
prettyPrintPiece (CP White Bishop) = "♗"
prettyPrintPiece (CP White Knight) = "♘"
prettyPrintPiece (CP White (Pawn _)) = "♙"
prettyPrintPiece Null = ""

readBoard :: Text -> Either String Board
readBoard boardStr = Right $ map readRow $ chunksOf 8 boardStr

readRow :: Text -> [CPiece]
readRow rowStr = map (unprintPiece . unpack) $ chunksOf 1 rowStr

writeBoard :: Board -> String
writeBoard = concatMap printRow

moveToCastling :: Move -> Maybe CMove
moveToCastling move = case move of
  ((5, 8), (7, 8)) -> Just $ Castling Black Short
  ((5, 8), (3, 8)) -> Just $ Castling Black Long
  ((5, 1), (7, 1)) -> Just $ Castling White Short
  ((5, 1), (3, 1)) -> Just $ Castling White Long
  _ -> Nothing

isCastling :: Board -> Color -> Move -> Bool
isCastling board color move =
  case moveToCastling move of
    Just (Castling clr side) ->
      let row = case clr of
            Black -> 8
            White -> 1
          (rookColumn, intermediateColumns) = case side of
            Short -> (8, [7, 6])
            Long -> (1, [2, 3, 4])
          king = getPiece board (5, row)
          rook = getPiece board (rookColumn, row)
       in color == clr
            && king == CP clr (King False)
            && rook == CP clr (Rook False)
            && all (\c -> getPiece board (c, row) == Null) intermediateColumns
            && all (\c -> not $ isPositionUnderAttack board clr (c, row)) (5 : intermediateColumns)
    _ -> False

isEnPassant :: Board -> Color -> Move -> Bool
isEnPassant board color ((sc, sr), (ec, er)) =
  let cdiff = ec - sc
      rdiff = er - sr
      attack = abs cdiff == 1 && rdiff == (if color == Black then -1 else 1)
   in attack
        && getPiece board (ec, er) == Null
        && getPiece board (ec, er + (if color == Black then 1 else -1)) == CP (swapColor color) (Pawn True)

isValidMove :: Board -> Color -> Bool -> Move -> Bool
isValidMove board color includesCastling (start, end) =
  start /= end
    && let startPiece = getPiece board start
        in case startPiece of
             CP startColor piece ->
               let predicate = case piece of
                     King False ->
                       if includesCastling
                         then isCastling board color
                         else const False
                     Pawn _ -> isEnPassant board color
                     _ -> const False
                in color == startColor
                     && (predicate (start, end) || isValidMoveInner board color (start, end) startPiece)
             Null -> False

isValidMoveInner :: Board -> Color -> Move -> CPiece -> Bool
isValidMoveInner board startColor (start, end) startPiece = case getPiece board end of
  Null ->
    isValidMovePiece startPiece False (start, end)
      && checkLineOfSight board startPiece (start, end)
  CP endColor _ ->
    startColor /= endColor
      && isValidMovePiece startPiece True (start, end)
      && checkLineOfSight board startPiece (start, end)

checkLineOfSight :: Board -> CPiece -> Move -> Bool
checkLineOfSight _ Null _ = False
checkLineOfSight _ (CP _ (King _)) _ = True
checkLineOfSight board (CP color Queen) move =
  checkLineOfSight board (CP color (Rook False)) move
    || checkLineOfSight board (CP color Bishop) move
checkLineOfSight board (CP _ (Rook _)) (start, end) = checkLineOfSightPos board (mkPositions start end)
checkLineOfSight board (CP _ Bishop) (start, end) = checkLineOfSightPos board (mkPositions start end)
checkLineOfSight _ (CP _ Knight) _ = True
checkLineOfSight board (CP _ (Pawn _)) ((sc, sr), (_, er))
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
setPiece board (cn, rn) piece = fst $ alterBoardRow board rn (\r -> alterRow r cn (const piece))

removePiece :: Board -> Position -> (Board, CPiece)
removePiece board (cn, rn) = alterBoardRow board rn (\r -> alterRow r cn (const Null))

movePiece :: Board -> CMove -> Board
movePiece board (Normal (start, end)) =
  let (b1, oldPiece) = removePiece board start
      rdiff ((_, sr), (_, er)) = abs (er - sr) == 2
      newPiece = case oldPiece of
        CP color (King False) -> CP color (King True)
        CP color (Rook False) -> CP color (Rook True)
        CP color (Pawn False) -> CP color (Pawn $ rdiff (start, end))
        _ -> oldPiece
   in setPiece b1 end newPiece
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
movePiece board (EnPassant color (start, end@(ec, er))) =
  let (b1, oldPiece) = removePiece board start
      (b2, _) = removePiece b1 (ec, er + (if color == Black then 1 else -1))
   in setPiece b2 end oldPiece

advanceBoard :: Board -> Color -> Move -> Either String Board
advanceBoard board color move@(start, _) =
  let cmove = case getPiece board start of
        CP _ (King False) ->
          if isCastling board color move
            then fromMaybe (Normal move) $ moveToCastling move
            else Normal move
        CP _ (Pawn _) ->
          if isEnPassant board color move
            then EnPassant color move
            else Normal move
        _ -> Normal move
   in if isValidMove board color True move
        then Right $ movePiece board cmove
        else Left $ "ERROR: Invalid move: " ++ show cmove

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

genPossibleMovesPiece :: Board -> Bool -> Position -> [Move]
genPossibleMovesPiece board includesCastling position = case getPiece board position of
  CP color _ -> filter (isValidMove board color includesCastling) (map (\pos -> (position, pos)) mkCoords)
  _ -> []

genPossibleMoves :: Board -> Color -> Bool -> [Move]
genPossibleMoves board color includesCastling = concatMap (genPossibleMovesPiece board includesCastling) (getPositions board color)

isPositionUnderAttack :: Board -> Color -> Position -> Bool
isPositionUnderAttack board color pos = elem pos $ map snd $ genPossibleMoves board (swapColor color) False

promotePawn :: [CPiece] -> [CPiece]
promotePawn [] = []
promotePawn (CP color (Pawn _) : ps) = CP color Queen : promotePawn ps
promotePawn (p : ps) = p : promotePawn ps

promotePawns :: Board -> Board
promotePawns board =
  let firstRow = head board
      lastRow = board !! 7
   in promotePawn firstRow : take 6 (tail board) ++ [promotePawn lastRow]

resetPawns :: Board -> Color -> Board
resetPawns board color =
  let resetPawn piece@(CP clr (Pawn True)) = if color == clr then CP color (Pawn False) else piece
      resetPawn piece = piece
   in map (map resetPawn) board
