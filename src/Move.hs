module Move
  ( readMove,
    writeMove,
    validMovePiece,
  )
where

import Data.Char (intToDigit)
import Position (isValidPosition, mkPosition)
import Types (CPiece (..), Color (..), Move, Piece (..))

readMove :: String -> Either String Move
readMove moveStr@(sc : sr : ec : er : _) =
  let start = mkPosition (sc, sr)
      end = mkPosition (ec, er)
   in if isValidPosition start && isValidPosition end
        then Right (start, end)
        else Left $ "ERROR: Invalid move string: " ++ moveStr
readMove moveStr = Left $ "ERROR: Invalid move string: " ++ moveStr

writeMove :: Move -> String
writeMove ((sc, sr), (ec, er)) = intToDigit sc : intToDigit sr : intToDigit ec : intToDigit er : []

validMovePiece :: CPiece -> Bool -> Move -> Bool
validMovePiece (CP Black Pawn) False ((sc, sr), (ec, er)) =
  let cdiff = ec - sc
      rdiff = er - sr
   in cdiff == 0 && (rdiff == (-1) || (sr == 7 && rdiff == (-2)))
validMovePiece (CP White Pawn) False ((sc, sr), (ec, er)) =
  let cdiff = ec - sc
      rdiff = er - sr
   in cdiff == 0 && (rdiff == 1 || (sr == 2 && rdiff == 2))
validMovePiece piece attack ((sc, sr), (ec, er)) = validMovePieceInner piece attack (ec - sc, er - sr)

validMovePieceInner :: CPiece -> Bool -> (Int, Int) -> Bool
validMovePieceInner (CP _ King) _ (cdiff, rdiff) =
  (cdiff == 0 && abs rdiff == 1)
    || (rdiff == 0 && abs cdiff == 1)
    || (abs cdiff == abs rdiff && abs cdiff == 1)
validMovePieceInner (CP color Queen) attack move =
  validMovePieceInner (CP color Rook) attack move
    || validMovePieceInner (CP color Bishop) attack move
validMovePieceInner (CP _ Rook) _ (cdiff, rdiff) = cdiff == 0 || rdiff == 0
validMovePieceInner (CP _ Bishop) _ (cdiff, rdiff) = abs cdiff == abs rdiff
validMovePieceInner (CP _ Knight) _ (cdiff, rdiff) =
  (abs cdiff == 1 && abs rdiff == 2)
    || (abs cdiff == 2 && abs rdiff == 1)
validMovePieceInner (CP Black Pawn) True (cdiff, rdiff) = abs cdiff == 1 && rdiff == (-1)
validMovePieceInner (CP White Pawn) True (cdiff, rdiff) = abs cdiff == 1 && rdiff == 1
validMovePieceInner _ _ _ = False
