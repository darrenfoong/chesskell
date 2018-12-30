import Data.Char
import Control.Monad
import System.Random

data Piece  = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq
data Color  = Black | White deriving (Eq, Show)
data CPiece = CP Color Piece | Null

type Board    = [[CPiece]]
type Position = (Int, Int)
type Move     = (Position, Position)

main :: IO ()
main = do
         gen <- getStdGen
         loopBoard gen mkBoard White

mkBoard :: Board
mkBoard = [mkMixedRow White,
           mkPawnRow White,
           mkBlankRow,
           mkBlankRow,
           mkBlankRow,
           mkBlankRow,
           mkPawnRow Black,
           mkMixedRow Black]

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

mkCoords :: [Position]
mkCoords = [(c,r) | c <- [1..8], r <- [1..8]]

printBoard :: Board -> Bool -> String
printBoard board debug = let result = map (\(r,n) -> show n ++ " " ++ printRow r) $ f $ zip board ([1..8] :: [Int]) in
                           unlines $ result ++ extraLines
                         where f = if debug then id else reverse
                               extraLines = if debug then ["  12345678", "  abcdefgh"] else ["  abcdefgh"]

printRow :: [CPiece] -> String
printRow = concatMap printPiece

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

scorePiece :: CPiece -> Int
scorePiece (CP _ King)   = 0
scorePiece (CP _ Queen)  = 9
scorePiece (CP _ Rook)   = 5
scorePiece (CP _ Bishop) = 3
scorePiece (CP _ Knight) = 3
scorePiece (CP _ Pawn)   = 1
scorePiece Null          = 0

scoreColorPiece :: Color -> CPiece -> Int
scoreColorPiece color p@(CP pcolor _) = if color == pcolor then scorePiece p else 0
scoreColorPiece _ Null = 0

scoreBoard :: Color -> Board -> Int
scoreBoard color = sum . map (sum . map (scoreColorPiece color))

makePosition :: (Char, Char) -> Position
makePosition (c,n) = (ord c - ord 'a' + 1, digitToInt n)

parseMove :: String -> Maybe Move
parseMove (sc:sr:ec:er:_) = let start = makePosition (sc,sr)
                                end = makePosition (ec,er) in
                                if validPos start && validPos end
                                then Just (start, end)
                                else Nothing
parseMove _ = Nothing

swapColor :: Color -> Color
swapColor Black = White
swapColor White = Black

validPos :: Position -> Bool
validPos (c,r) = (1 <= c) && (c <= 8) && (1 <= r) && (r <= 8)

-- todo: checkmate check

validMove :: Board -> Move -> Color -> Bool
validMove board (start, end) color = start /= end &&
                                     let startPiece = getPiece board start in
                                       case startPiece of
                                         CP startColor _ ->
                                           startColor == color &&
                                           case getPiece board end of
                                                  Null          -> validMovePiece startPiece False (start, end) &&
                                                                   checkLineOfSight board startPiece (start, end)
                                                  CP endColor _ -> startColor /= endColor &&
                                                                   validMovePiece startPiece True (start, end) &&
                                                                   checkLineOfSight board startPiece (start, end)
                                         Null -> False

validMovePiece :: CPiece -> Bool -> Move -> Bool
validMovePiece Null            _      _                  = False
validMovePiece (CP _ King)     _     ((sc,sr), (ec,er)) = (ec == sc && abs(er-sr) == 1) ||
                                                          (er == sr && abs(ec-sc) == 1) ||
                                                          ((ec-sc) == (er-sr) && abs(ec-sc) == 1)
validMovePiece (CP color Queen) attack move             = validMovePiece (CP color Rook) attack move ||
                                                          validMovePiece (CP color Bishop) attack move
validMovePiece (CP _ Rook)     _     ((sc,sr), (ec,er)) = ec == sc || er == sr
validMovePiece (CP _ Bishop)   _     ((sc,sr), (ec,er)) = abs(ec-sc) == abs(er-sr)
validMovePiece (CP _ Knight)   _     ((sc,sr), (ec,er)) = let cdiff = (ec-sc)
                                                              rdiff = (er-sr) in
                                                              (abs(cdiff) == 1 && abs(rdiff) == 2) ||
                                                              (abs(cdiff) == 2 && abs(rdiff) == 1)
validMovePiece (CP Black Pawn) True  ((sc,sr), (ec,er)) = abs(ec-sc) == 1 && (er-sr) == (-1)
validMovePiece (CP White Pawn) True  ((sc,sr), (ec,er)) = abs(ec-sc) == 1 && (er-sr) == 1
validMovePiece (CP Black Pawn) False ((sc,sr), (ec,er)) = ec == sc && ((er-sr) == (-1) || (sr == 7 && (er-sr) == (-2)))
validMovePiece (CP White Pawn) False ((sc,sr), (ec,er)) = ec == sc && ((er-sr) == 1 || (sr == 2 && (er-sr) == 2))

checkLineOfSight :: Board -> CPiece -> Move -> Bool
checkLineOfSight _     Null          _                 = False
checkLineOfSight _     (CP _ King)   _                 = True
checkLineOfSight board (CP color Queen) move           = checkLineOfSight board (CP color Rook) move ||
                                                         checkLineOfSight board (CP color Bishop) move
checkLineOfSight board (CP _ Rook)   (start, end)      = checkLineOfSightPos board (mkPos start end)
checkLineOfSight board (CP _ Bishop) (start, end)      = checkLineOfSightPos board (mkPos start end)
checkLineOfSight _     (CP _ Knight) _                 = True
checkLineOfSight board (CP _ Pawn)   ((sc,sr), (_,er))
  | (er-sr) == 2  = checkLineOfSightPos board [(sc,sr+1)]
  | (er-sr) == -2 = checkLineOfSightPos board [(sc,sr-1)]
  | otherwise     = True

compareToInt :: Int -> Int -> Int
compareToInt a b
  | a > b     = -1
  | a < b     = 1
  | otherwise = 0

mkPos :: Position -> Position -> [Position]
mkPos (sc,sr) (ec,er) = let cdelta = compareToInt sc ec
                            rdelta = compareToInt sr er in
                          [(c,r) | c <- [sc,sc+cdelta..ec], r <- [sr,sr+rdelta..er]]

checkLineOfSightPos :: Board -> [Position] -> Bool
checkLineOfSightPos _     []     = True
checkLineOfSightPos board (p:ps) = case getPiece board p of
                                     Null -> checkLineOfSightPos board ps
                                     _    -> False

advanceBoard :: Board -> Move -> Color -> Maybe Board
advanceBoard board move color = if validMove board move color
                                then Just (movePiece board move)
                                else Nothing

movePiece :: Board -> Move -> Board
movePiece board (start, end) = let (intermediateBoard, oldPiece) = removePiece board start in
                                 setPiece intermediateBoard end oldPiece

alterBoardRow :: Board -> Int -> ([CPiece] -> ([CPiece], CPiece)) -> (Board, CPiece)
alterBoardRow []     _ _ = ([], Null)
alterBoardRow (r:rs) 1 f = let (alteredRow, oldPiece) = f r in
                             ((alteredRow:rs), oldPiece)
alterBoardRow (r:rs) n f = let (alteredBoard, oldPiece) = alterBoardRow rs (n-1) f in
                             (r:alteredBoard, oldPiece)

alterRow :: [CPiece] -> Int -> (CPiece -> CPiece) -> ([CPiece], CPiece)
alterRow []     _ _ = ([], Null)
alterRow (p:ps) 1 f = ((f p):ps, p)
alterRow (p:ps) n f = let (alteredRow, oldPiece) = alterRow ps (n-1) f in
                        (p:alteredRow, oldPiece)

getPiece :: Board -> Position -> CPiece
getPiece board (cn,rn) = board !! (rn-1) !! (cn-1)

setPiece :: Board -> Position -> CPiece -> Board
setPiece board (cn,rn) piece = let (alteredBoard, _) = alterBoardRow board rn (\r -> alterRow r cn (\_ -> piece)) in
                                 alteredBoard

removePiece :: Board -> Position -> (Board, CPiece)
removePiece board (cn,rn) = alterBoardRow board rn (\r -> alterRow r cn (\_ -> Null))

respondBoard :: StdGen -> Board -> Color -> (StdGen, Maybe Board)
respondBoard gen board color = let (newGen, maybeMove) = genMove gen board color in
                                 case maybeMove of
                                   Just m -> (newGen, advanceBoard board m color)
                                   Nothing -> (newGen, Nothing)

genMove :: StdGen -> Board -> Color -> (StdGen, Maybe Move)
genMove gen board color = case genMoves board color of
                        [] -> (gen, Nothing)
                        ms -> let (i, newGen) = randomR (1, length ms) gen in
                                (newGen, Just (ms !! (i-1)))


genMoves :: Board -> Color -> [Move]
genMoves board color = concat $ map (\position -> genPossibleMovesPiece board position) (genPositions board color)

genPositions :: Board -> Color -> [Position]
genPositions board color = foldl (\ps p -> case getPiece board p of
                                             CP clr _ -> if color == clr then p:ps else ps
                                             _        -> ps) [] mkCoords

genPossibleMovesPiece :: Board -> Position -> [Move]
genPossibleMovesPiece board position = case getPiece board position of
                                         CP color _ -> filter (\move -> validMove board move color) (map (\pos -> (position, pos)) mkCoords)
                                         _          -> []

loopBoard :: StdGen -> Board -> Color -> IO ()
loopBoard gen board color = forever $ do
                          putStrLn $ printBoard board True
                          putStrLn $ "Please enter your move (" ++ show color ++ "): "
                          moveStr <- getLine
                          case parseMove moveStr of
                            Just move -> case advanceBoard board move color of
                                           Just advancedBoard -> let (newGen, maybeBoard) = respondBoard gen advancedBoard $ swapColor color in
                                                                   case maybeBoard of
                                                                     Just respondedBoard -> loopBoard newGen respondedBoard color
                                                                     Nothing -> putStrLn $ "ERROR: Program has made an invalid move"
                                           Nothing -> putStrLn $ "ERROR: Invalid move: " ++ moveStr
                            Nothing -> putStrLn $ "ERROR: Invalid move string: " ++ moveStr
