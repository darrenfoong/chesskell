import System.Environment
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
parseMove (c1:n1:c2:n2:_) = let start = makePosition (c1,n1)  in
                            let end = makePosition (c2,n2) in
                              if validPos start && validPos end
                              then Just (start, end)
                              else Nothing
parseMove _ = Nothing

validPos :: Position -> Bool
validPos (c,r) = (1 <= c) && (c <= 8) && (1 <= r) && (r <= 8)

validMove :: Board -> Move -> Bool
validMove board (start, end) = True

advanceBoard :: Board -> Move -> Maybe Board
advanceBoard board move = if validMove board move
                          then Just (movePiece board move)
                          else Nothing

movePiece :: Board -> Move -> Board
movePiece board (start, end) = let (intermediateBoard, piece) = removePiece board start in
                                 addPiece intermediateBoard end piece

removePiece :: Board -> Position -> (Board, CPiece)
removePiece board start = (board, Null)

addPiece :: Board -> Position -> CPiece -> Board
addPiece board end piece = board

loopBoard :: Board -> IO ()
loopBoard board = do
                    putStrLn (printBoard board)
                    putStrLn "Please enter your move (black): "
                    move <- getLine
                    let parsedMove = parseMove move in
                      case parsedMove of
                        Just mv -> let advancedBoard = advanceBoard board mv in
                                     case advancedBoard of
                                       Just bd -> loopBoard bd
                                       Nothing -> do
                                                    putStrLn ("ERROR: Invalid move: " ++ move)
                                                    loopBoard board
                        Nothing -> do
                                     putStrLn ("ERROR: Invalid move string: " ++ move)
                                     loopBoard board
