module Board
  ( mkBoard,
    mkPos,
    mkCoords,
    printBoard,
    scoreBoard,
    advanceBoard,
    parseMove,
    movePiece,
    getPiece,
    validMove,
  )
where

import Move (parseMove, validMovePiece)
import Position (mkPos, mkPositions)
import Types (Board, CPiece (..), Color (..), Move, Piece (..), Position)

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

scoreBoard :: Color -> Board -> Int
scoreBoard color = sum . map (sum . map (scoreColorPiece color))

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

-- todo: checkmate check

validMove :: Board -> Move -> Color -> Bool
validMove board (start, end) color =
  start /= end
    && let startPiece = getPiece board start
        in case startPiece of
             CP startColor _ ->
               startColor == color
                 && case getPiece board end of
                   Null ->
                     validMovePiece startPiece False (start, end)
                       && checkLineOfSight board startPiece (start, end)
                   CP endColor _ ->
                     startColor /= endColor
                       && validMovePiece startPiece True (start, end)
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
checkLineOfSightPos _ [] = True
checkLineOfSightPos board (p : ps) = case getPiece board p of
  Null -> checkLineOfSightPos board ps
  _ -> False

advanceBoard :: Board -> Move -> Color -> Either String Board
advanceBoard board move color =
  if validMove board move color
    then Right $ movePiece board move
    else Left $ "ERROR: Invalid move: " ++ show move
