import Board (advanceBoard, mkBoard, printBoard)
import Control.Monad
import Logic (respondBoard)
import Move (parseMove)
import System.Random
import Types (Board, Color (..), swapColor)

main :: IO ()
main = do
  gen <- getStdGen
  loopBoard gen mkBoard White

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
  let (newGen, mBoard) = respondBoard gen advancedBoard $ swapColor color
   in do
        respondedBoard <- mBoard
        return (newGen, respondedBoard)
