import System.Environment

data Piece  = King | Queen | Rook | Bishop | Knight | Pawn
data Color  = Black | White
data CPiece = CP Color Piece | Null

type Board    = [[CPiece]]
type Position = (Integer, Integer)
type Move     = (Position, Position)

main :: IO ()
main = let board = mkBoard in
         loopBoard board

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

parseMove :: String -> Move
parseMove moveStr = ((0,0), (0,0))

advanceBoard :: Board -> Move -> Board
advanceBoard board move = board

loopBoard :: Board -> IO ()
loopBoard board = do
                    putStrLn (printBoard board)
                    putStrLn "Please enter your move (black): "
                    move <- getLine
                    loopBoard (advanceBoard board (parseMove move))
