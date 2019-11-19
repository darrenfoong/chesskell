import Data.List
import Control.Monad
import System.Random

import Types (Piece(..), Color(..), CPiece(..), Board, Position, Move, swapColor)
import Board (mkBoard, mkPos, mkCoords, scoreBoard, printBoard)

main :: IO ()
main = do
         gen <- getStdGen
         loopBoard gen mkBoard White

parseMove :: String -> Either String Move
parseMove moveStr@(sc:sr:ec:er:_) = let start = mkPos (sc,sr)
                                        end = mkPos (ec,er) in
                                if validPos start && validPos end
                                then Right (start, end)
                                else Left $ "ERROR: Invalid move string: " ++ moveStr
parseMove moveStr = Left $ "ERROR: Invalid move string: " ++ moveStr

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
                                                              (abs cdiff == 1 && abs rdiff == 2) ||
                                                              (abs cdiff == 2 && abs rdiff == 1)
validMovePiece (CP Black Pawn) True  ((sc,sr), (ec,er)) = abs(ec-sc) == 1 && (er-sr) == (-1)
validMovePiece (CP White Pawn) True  ((sc,sr), (ec,er)) = abs(ec-sc) == 1 && (er-sr) == 1
validMovePiece (CP Black Pawn) False ((sc,sr), (ec,er)) = ec == sc && ((er-sr) == (-1) || (sr == 7 && (er-sr) == (-2)))
validMovePiece (CP White Pawn) False ((sc,sr), (ec,er)) = ec == sc && ((er-sr) == 1 || (sr == 2 && (er-sr) == 2))

checkLineOfSight :: Board -> CPiece -> Move -> Bool
checkLineOfSight _     Null          _                 = False
checkLineOfSight _     (CP _ King)   _                 = True
checkLineOfSight board (CP color Queen) move           = checkLineOfSight board (CP color Rook) move ||
                                                         checkLineOfSight board (CP color Bishop) move
checkLineOfSight board (CP _ Rook)   (start, end)      = checkLineOfSightPos board (mkPositions start end)
checkLineOfSight board (CP _ Bishop) (start, end)      = checkLineOfSightPos board (mkPositions start end)
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

mkPositions :: Position -> Position -> [Position]
mkPositions (sc,sr) (ec,er) = let cdelta = compareToInt sc ec
                                  rdelta = compareToInt sr er in
                                [(c,r) | c <- [sc,sc+cdelta..ec], r <- [sr,sr+rdelta..er]]

checkLineOfSightPos :: Board -> [Position] -> Bool
checkLineOfSightPos _     []     = True
checkLineOfSightPos board (p:ps) = case getPiece board p of
                                     Null -> checkLineOfSightPos board ps
                                     _    -> False

advanceBoard :: Board -> Move -> Color -> Either String Board
advanceBoard board move color = if validMove board move color
                                then Right $ movePiece board move
                                else Left $ "ERROR: Invalid move: " ++ show move

movePiece :: Board -> Move -> Board
movePiece board (start, end) = let (intermediateBoard, oldPiece) = removePiece board start in
                                 setPiece intermediateBoard end oldPiece

alterBoardRow :: Board -> Int -> ([CPiece] -> ([CPiece], CPiece)) -> (Board, CPiece)
alterBoardRow []     _ _ = ([], Null)
alterBoardRow (r:rs) 1 f = let (alteredRow, oldPiece) = f r in
                             (alteredRow:rs, oldPiece)
alterBoardRow (r:rs) n f = let (alteredBoard, oldPiece) = alterBoardRow rs (n-1) f in
                             (r:alteredBoard, oldPiece)

alterRow :: [CPiece] -> Int -> (CPiece -> CPiece) -> ([CPiece], CPiece)
alterRow []     _ _ = ([], Null)
alterRow (p:ps) 1 f = (f p : ps, p)
alterRow (p:ps) n f = let (alteredRow, oldPiece) = alterRow ps (n-1) f in
                        (p:alteredRow, oldPiece)

getPiece :: Board -> Position -> CPiece
getPiece board (cn,rn) = board !! (rn-1) !! (cn-1)

setPiece :: Board -> Position -> CPiece -> Board
setPiece board (cn,rn) piece = let (alteredBoard, _) = alterBoardRow board rn (\r -> alterRow r cn (const piece)) in
                                 alteredBoard

removePiece :: Board -> Position -> (Board, CPiece)
removePiece board (cn,rn) = alterBoardRow board rn (\r -> alterRow r cn (const Null))

respondBoard :: StdGen -> Board -> Color -> (StdGen, Either String Board)
respondBoard gen board color = let (newGen, maybeMove) = genMove gen board color in
                                 case maybeMove of
                                   Just m -> (newGen, advanceBoard board m color)
                                   Nothing -> (newGen, Left "ERROR: Program has made an invalid move")

genMove :: StdGen -> Board -> Color -> (StdGen, Maybe Move)
genMove gen board color = let (_,m) = minimax board color color 3 True in
                            (gen, Just m)

minimax :: Board -> Color -> Color -> Int -> Bool -> (Int, Move)
minimax board scoringColor playerColor 1 maximising = maximumBy (compareMove maximising) $ map (\m -> (scoreBoard scoringColor (movePiece board m), m)) $ genMoves board playerColor
minimax board scoringColor playerColor n maximising = maximumBy (compareMove maximising) $ map (\m -> let (s,_) = minimax (movePiece board m) scoringColor (swapColor playerColor) (n-1) (not maximising) in (s,m)) $ genMoves board playerColor

compareMove :: Bool -> (Int, Move) -> (Int, Move) -> Ordering
compareMove maximising (s1,_) (s2,_) = if maximising then compare s1 s2 else compare s2 s1

genMoves :: Board -> Color -> [Move]
genMoves board color = concatMap (genPossibleMovesPiece board) (genPositions board color)

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
                          case loopBoardInner gen board color moveStr of
                            Left err -> putStrLn err
                            Right (newGen, respondedBoard) -> loopBoard newGen respondedBoard color

loopBoardInner :: StdGen -> Board -> Color -> String -> Either String (StdGen, Board)
loopBoardInner gen board color moveStr = do
                            move <- parseMove moveStr
                            advancedBoard <- advanceBoard board move color
                            let (newGen, maybeBoard) = respondBoard gen advancedBoard $ swapColor color in
                              do
                                respondedBoard <- maybeBoard
                                return (newGen, respondedBoard)
