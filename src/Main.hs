import Control.Monad
import System.Random

import Types (Color(..), Board, swapColor)
import Board (mkBoard, printBoard, advanceBoard, parseMove)
import Logic (respondBoard)

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
                            let (newGen, maybeBoard) = respondBoard gen advancedBoard $ swapColor color in
                              do
                                respondedBoard <- maybeBoard
                                return (newGen, respondedBoard)
