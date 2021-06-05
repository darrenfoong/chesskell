module Move
  ( readMove,
    writeMove,
    isValidMovePiece,
  )
where

import Data.Char (chr, intToDigit, ord)
import Position (isValidPosition, mkPosition)
import Types (CMove (..), CPiece (..), Color (..), Move, Piece (..))

readMove :: String -> Either String CMove
readMove moveStr@(sc : sr : ec : er : _) =
  let start = mkPosition (sc, sr)
      end = mkPosition (ec, er)
   in if isValidPosition start && isValidPosition end
        then Right $ Normal (start, end)
        else Left $ "ERROR: Invalid move string: " ++ moveStr
readMove moveStr = Left $ "ERROR: Invalid move string: " ++ moveStr

writeMove :: CMove -> String
writeMove (Normal ((sc, sr), (ec, er))) =
  let f n = chr $ ord 'a' + n - 1
   in [f sc, intToDigit sr, f ec, intToDigit er]
writeMove (Castling _ _) = "" -- TODO

isValidMovePiece :: CPiece -> Bool -> Move -> Bool
isValidMovePiece (CP Black Pawn) False ((sc, sr), (ec, er)) =
  let cdiff = ec - sc
      rdiff = er - sr
   in cdiff == 0 && (rdiff == (-1) || (sr == 7 && rdiff == (-2)))
isValidMovePiece (CP White Pawn) False ((sc, sr), (ec, er)) =
  let cdiff = ec - sc
      rdiff = er - sr
   in cdiff == 0 && (rdiff == 1 || (sr == 2 && rdiff == 2))
isValidMovePiece piece attack ((sc, sr), (ec, er)) = isValidMovePieceInner piece attack (ec - sc, er - sr)

isValidMovePieceInner :: CPiece -> Bool -> (Int, Int) -> Bool
isValidMovePieceInner (CP _ (King _)) _ (cdiff, rdiff) =
  (cdiff == 0 && abs rdiff == 1)
    || (rdiff == 0 && abs cdiff == 1)
    || (abs cdiff == abs rdiff && abs cdiff == 1)
isValidMovePieceInner (CP color Queen) attack move =
  isValidMovePieceInner (CP color (Rook False)) attack move
    || isValidMovePieceInner (CP color Bishop) attack move
isValidMovePieceInner (CP _ (Rook _)) _ (cdiff, rdiff) = cdiff == 0 || rdiff == 0
isValidMovePieceInner (CP _ Bishop) _ (cdiff, rdiff) = abs cdiff == abs rdiff
isValidMovePieceInner (CP _ Knight) _ (cdiff, rdiff) =
  (abs cdiff == 1 && abs rdiff == 2)
    || (abs cdiff == 2 && abs rdiff == 1)
isValidMovePieceInner (CP Black Pawn) True (cdiff, rdiff) = abs cdiff == 1 && rdiff == (-1)
isValidMovePieceInner (CP White Pawn) True (cdiff, rdiff) = abs cdiff == 1 && rdiff == 1
isValidMovePieceInner _ _ _ = False
