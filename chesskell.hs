import Data.Char

data Piece  = King | Queen | Rook | Bishop | Knight | Pawn
data Color  = Black | White
data CPiece = CP Color Piece | Null

type Board    = [[CPiece]]
type Position = (Int, Int)
type Move     = (Position, Position)

main :: IO ()
main = loopBoard mkBoard

mkBoard :: Board
mkBoard = [mkMixedRow Black,
           mkPawnRow Black,
           mkBlankRow,
           mkBlankRow,
           mkBlankRow,
           mkBlankRow,
           mkPawnRow White,
           mkMixedRow White]

mkMixedRow :: Color -> [CPiece]
mkMixedRow color = [CP color Rook,
                    CP color Knight,
                    CP color Bishop,
                    CP color Queen,
                    CP color King,
                    CP color Bishop,
                    CP color Knight,
                    CP color Rook]

mkPawnRow :: Color -> [CPiece]
mkPawnRow color = replicate 8 (CP color Pawn)

mkBlankRow :: [CPiece]
mkBlankRow = replicate 8 Null

printBoard :: Board -> String
printBoard board = printBoardInner board 8

printBoardInner :: Board -> Int -> String
printBoardInner (r:rs) n = (show n) ++ " " ++
                           (printRow r) ++ "\n" ++
                           printBoardInner rs (n-1)
printBoardInner [] _ = "  abcdefgh"

printRow :: [CPiece] -> String
printRow row = foldl (\e x -> e ++ (printPiece x)) "" row

printPiece :: CPiece -> String
printPiece (CP Black King)   = "K"
printPiece (CP Black Queen)  = "Q"
printPiece (CP Black Rook)   = "R"
printPiece (CP Black Bishop) = "B"
printPiece (CP Black Knight) = "N"
printPiece (CP Black Pawn)   = "P"
printPiece (CP White King)   = "k"
printPiece (CP White Queen)  = "q"
printPiece (CP White Rook)   = "r"
printPiece (CP White Bishop) = "b"
printPiece (CP White Knight) = "n"
printPiece (CP White Pawn)   = "p"
printPiece Null              = "#"

makePosition :: (Char, Char) -> Position
makePosition (c,n) = (ord c - ord 'a' + 1, digitToInt n)

parseMove :: String -> Maybe Move
parseMove (sc:sr:ec:er:_) = let start = makePosition (sc,sr) in
                              let end = makePosition (ec,er) in
                                if validPos start && validPos end
                                then Just (start, end)
                                else Nothing
parseMove _ = Nothing

compareColors :: Color -> Color -> Bool
compareColors Black Black = False
compareColors White White = False
compareColors _     _     = True

validPos :: Position -> Bool
validPos (c,r) = (1 <= c) && (c <= 8) && (1 <= r) && (r <= 8)

-- todo: line of sight check
-- todo: checkmate check

validMove :: Board -> Move -> Bool
validMove board (start, end) = start /= end &&
                               let startPiece = getPiece board start in
                                 case getPiece board end of
                                    Null          -> validMovePiece startPiece False (start, end) &&
                                                     checkLineOfSight board startPiece (start, end)
                                    CP endColor _ -> let CP startColor _ = startPiece in
                                                       compareColors startColor endColor &&
                                                       validMovePiece startPiece True (start, end) &&
                                                       checkLineOfSight board startPiece (start, end)

validMovePiece :: CPiece -> Bool -> Move -> Bool
validMovePiece Null          _   _                  = False
validMovePiece (CP _ King)   _   ((sc,sr), (ec,er)) = ((sc-ec) == 0 && abs(er-sr) == 1) ||
                                                      ((er-sr) == 0 && abs(sc-ec) == 1) ||
                                                      ((sc-ec) == (er-sr) && abs(sc-ec) == 1)
validMovePiece (CP color Queen) attack move = validMovePiece (CP color Rook) attack move ||
                                                      validMovePiece (CP color Bishop) attack move
validMovePiece (CP _ Rook)   _   ((sc,sr), (ec,er)) = (sc-ec) == 0 || (er-sr) == 0
validMovePiece (CP _ Bishop) _   ((sc,sr), (ec,er)) = (sc-ec) == (er-sr)
validMovePiece (CP _ Knight) _   ((sc,sr), (ec,er)) = let cdiff = (sc-ec) in
                                                        let rdiff = (er-sr) in
                                                          (abs(cdiff) == 1 && abs(rdiff) == 2) ||
                                                          (abs(cdiff) == 2 && abs(rdiff) == 1)
validMovePiece (CP Black Pawn) True  ((sc,sr), (ec,er)) = abs(sc-ec) == 1 && (er-sr) == (-1)
validMovePiece (CP White Pawn) True  ((sc,sr), (ec,er)) = abs(sc-ec) == 1 && (er-sr) == 1
validMovePiece (CP Black Pawn) False ((sc,sr), (ec,er)) = (sc-ec) == 0 && ((er-sr) == (-1) || (sr == 7 && (er-sr) == (-2)))
validMovePiece (CP White Pawn) False ((sc,sr), (ec,er)) = (sc-ec) == 0 && ((er-sr) == 1 || (sr == 2 && (er-sr) == 2))

checkLineOfSight :: Board -> CPiece -> Move -> Bool
checkLineOfSight _ Null _ = False
checkLineOfSight _ (CP _ King) _ = True
checkLineOfSight board (CP color Queen) move = checkLineOfSight board (CP color Rook) move ||
                                               checkLineOfSight board (CP color Bishop) move
checkLineOfSight board (CP _ Rook) (start, end) = checkLineOfSightPos board (mkPos start end)
checkLineOfSight board (CP _ Bishop) (start, end) = checkLineOfSightPos board (mkPos start end)
checkLineOfSight _ (CP _ Knight) _ = True
checkLineOfSight board (CP _ Pawn) ((sc,sr), (_,er)) = if (er-sr) == 2
                                                        then checkLineOfSightPos board [(sc,sr+1)]
                                                        else if (er-sr) == -2
                                                             then checkLineOfSightPos board [(sc,sr-1)]
                                                             else True

compareToInt :: Int -> Int -> Int
compareToInt a b = if a > b
                   then 1
                   else if a < b
                        then -1
                        else 0

mkPos :: Position -> Position -> [Position]
mkPos (sc,sr) (ec,er) = let cdelta = compareToInt sc ec in
                          let rdelta = compareToInt er sr in
                             mkPosInner (sc,sr) (ec,er) cdelta rdelta

mkPosInner :: Position -> Position -> Int -> Int -> [Position]
mkPosInner (sc,sr) (ec,er) cdelta rdelta = if sc+cdelta == ec && sr+rdelta == er
                                           then []
                                           else let newStart = (sc+cdelta,sr+rdelta) in
                                                  newStart : mkPosInner newStart (ec,er) cdelta rdelta

checkLineOfSightPos :: Board -> [Position] -> Bool
checkLineOfSightPos _     []     = True
checkLineOfSightPos board (p:ps) = case getPiece board p of
                                     Null -> checkLineOfSightPos board ps
                                     _    -> False

advanceBoard :: Board -> Move -> Maybe Board
advanceBoard board move = if validMove board move
                          then Just (movePiece board move)
                          else Nothing

movePiece :: Board -> Move -> Board
movePiece board (start, end) = let (intermediateBoard, oldPiece) = removePiece board start in
                                 setPiece intermediateBoard end oldPiece

alterBoardRow :: Board -> Int -> ([CPiece] -> ([CPiece], CPiece)) -> (Board, CPiece)
alterBoardRow []     _ _ = ([], Null)
alterBoardRow (r:rs) 8 f = let (alteredRow, oldPiece) = f r in
                             ((alteredRow:rs), oldPiece)
alterBoardRow (r:rs) n f = let (alteredBoard, oldPiece) = alterBoardRow rs (n+1) f in
                             (r:alteredBoard, oldPiece)

alterRow :: [CPiece] -> Int -> (CPiece -> CPiece) -> ([CPiece], CPiece)
alterRow []     _ _ = ([], Null)
alterRow (p:ps) 1 f = ((f p):ps, p)
alterRow (p:ps) n f = let (alteredRow, oldPiece) = alterRow ps (n-1) f in
                               (p:alteredRow, oldPiece)

getPiece :: Board -> Position -> CPiece
getPiece board (cn,rn) = let (_, oldPiece) = alterBoardRow board rn (\r -> alterRow r cn (\p -> p)) in
                           oldPiece

setPiece :: Board -> Position -> CPiece -> Board
setPiece board (cn,rn) piece = let (alteredBoard, _) = alterBoardRow board rn (\r -> alterRow r cn (\_ -> piece)) in
                                 alteredBoard

removePiece :: Board -> Position -> (Board, CPiece)
removePiece board (cn,rn) = alterBoardRow board rn (\r -> alterRow r cn (\_ -> Null))

loopBoard :: Board -> IO ()
loopBoard board = do
                    putStrLn (printBoard board)
                    putStrLn "Please enter your move (black): "
                    moveStr <- getLine
                    case parseMove moveStr of
                      Just move -> case advanceBoard board move of
                                     Just advancedBoard -> loopBoard advancedBoard
                                     Nothing -> do
                                                  putStrLn ("ERROR: Invalid move: " ++ moveStr)
                                                  loopBoard board
                      Nothing -> do
                                   putStrLn ("ERROR: Invalid move string: " ++ moveStr)
                                   loopBoard board
