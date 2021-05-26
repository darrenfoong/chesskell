import Board (advanceBoard, mkBoard, printBoard)
import Control.Monad
import Logic (genMove)
import Move (readMove)
import Scoring (scoreBoard)
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
  move <- readMove moveStr
  advancedBoard <- advanceBoard board color move
  let (newGen, mBoard) = respondBoard scoreBoard gen advancedBoard $ swapColor color
   in do
        respondedBoard <- mBoard
        return (newGen, respondedBoard)

respondBoard :: (Board -> Color -> Int) -> StdGen -> Board -> Color -> (StdGen, Either String Board)
respondBoard boardScorer gen board color =
  let (newGen, mMove) = genMove boardScorer gen board color
   in case mMove of
        Just m -> (newGen, advanceBoard board color m)
        Nothing -> (newGen, Left "ERROR: Program has made an invalid move")
