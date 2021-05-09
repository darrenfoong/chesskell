{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Board (advanceBoard, getPiece, mkBoard, parseMove)
import Control.Applicative ((<$>), (<*>))
import Data.Char
import Data.Maybe
import Data.Text (Text, append, pack, unpack)
import Logic (genMove, respondBoard)
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Types (Board, CPiece (..), Color (..), Move, Piece (..), swapColor)
import Yesod
import Yesod.Form

data App = App

data State = Start | PendingMoveStart | PendingMoveEnd deriving (Eq)

prettyPrintPiece :: CPiece -> String
prettyPrintPiece (CP Black King) = "♚"
prettyPrintPiece (CP Black Queen) = "♛"
prettyPrintPiece (CP Black Rook) = "♜"
prettyPrintPiece (CP Black Bishop) = "♝"
prettyPrintPiece (CP Black Knight) = "♞"
prettyPrintPiece (CP Black Pawn) = "♟︎"
prettyPrintPiece (CP White King) = "♔"
prettyPrintPiece (CP White Queen) = "♕"
prettyPrintPiece (CP White Rook) = "♖"
prettyPrintPiece (CP White Bishop) = "♗"
prettyPrintPiece (CP White Knight) = "♘"
prettyPrintPiece (CP White Pawn) = "♙"
prettyPrintPiece Types.Null = ""

mkYesod
  "App"
  [parseRoutes|
    / HomeR GET POST
    /oof NotFoundR GET
  |]

instance Yesod App where
  defaultLayout = appLayout
  errorHandler NotFound = redirect NotFoundR
  errorHandler other = defaultErrorHandler other

appLayout :: Widget -> Handler Html
appLayout widget = do
  pc <- widgetToPageContent $ do
    widget
    toWidgetHead [hamlet|<meta name="keywords" content="anki">|]
    toWidget
      [lucius|
        body { font: 1.0rem/1.1 sans-serif; }
        #content { padding: 10px; }
      |]
  withUrlRenderer
    [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle pc}
          ^{pageHead pc}
        <body>
          <div #content>
            ^{pageBody pc}
    |]

readBoard :: String -> Board
readBoard boardStr = mkBoard

writeBoard :: Board -> String
writeBoard board = ""

handleBoard :: StdGen -> Board -> Color -> String -> Either String Move
handleBoard gen board color moveStr = do
  move <- parseMove moveStr
  advancedBoard <- advanceBoard board move color
  let (newGen, mMove) = genMove gen board color
   in case mMove of
        Just m -> Right m
        Nothing -> Left "ERROR: Program has made an invalid move"

getHomeR = do
  mPosition <- lookupPostParam "position"
  mStart <- lookupPostParam "start"

  let state =
        if isNothing mStart && isNothing mPosition
          then Start
          else
            if isJust mStart && isJust mPosition
              then PendingMoveStart
              else PendingMoveEnd

  let mPreviousWhiteMove = case state of
        Start -> Nothing
        PendingMoveStart -> do
          start <- mStart
          end <- mPosition
          return $ unpack $ append start end
        PendingMoveEnd -> Nothing

  let gen = unsafePerformIO getStdGen -- TODO Fix this
  let previousBoard = case state of
        Start -> mkBoard
        _ -> readBoard ""
  let color = White

  let mPreviousBlackMove = case state of
        Start -> Nothing
        PendingMoveStart -> case mPreviousWhiteMove of
          Just previousWhiteMove -> case handleBoard gen previousBoard color previousWhiteMove of
            Left error -> Nothing -- TODO Handle this
            Right blackMove -> Just blackMove
          Nothing -> Nothing
        PendingMoveEnd -> Nothing -- TODO Carry over from previous state
  let nextBoard = case mPreviousBlackMove of
        Just previousBlackMove -> case advanceBoard previousBoard previousBlackMove (swapColor color) of
          Left error -> previousBoard -- TODO Handle this
          Right board -> board
        Nothing -> previousBoard

  defaultLayout $ do
    setTitle "chesskell"
    toWidget
      [lucius|
        #board div.row {
          clear: both
        }

        #board div.column {
          float: left;
        }

        #board div.row-label {
          float: left;
          width: 30px;
          height: 50px;
          line-height: 50px;
          text-align: center;
        }

        #board div.column-label {
          float: left;
          width: 50px;
          height: 30px;
          line-height: 30px;
          text-align: center;
        }

        #board div:nth-child(even) div:nth-child(odd) button, 
        #board div:nth-child(odd) div:nth-child(even) button {
          background-color: #ccc;
        }
        
        #board div:nth-child(even) div:nth-child(odd) button:hover, 
        #board div:nth-child(odd) div:nth-child(even) button:hover {
          background-color: #aaa;
        }
        
        #board button:hover {
          background-color: #aaa;
        }

        #board button {
          margin: 0;
          padding: 0;
          border: 1px solid #000;
          width: 50px;
          height: 50px;
          background-color: #fff;
          color: #000;
          font-size: 2.4rem;
          line-height: 50px;
          text-align: center;
        }
      |]
    [whamlet|
      <p>You are White.
      $if state /= Start
        $maybe previousBlackMove <- mPreviousBlackMove
          <p>Black played: #{show previousBlackMove}

      $if state /= PendingMoveEnd
        <p>Please select a piece.
      $else
        $maybe position <- mPosition
          <p>You selected <span>#{position}</span>. Please select where the piece should go.
    |]
    let cs = [1 .. 8]
    let cconv = \i -> chr (i + ord 'a' - 1)
    let rs = reverse [1 .. 8]
    [whamlet|
      <form method="post" action=@{HomeR}>
        $if state == PendingMoveEnd
          $maybe position <- mPosition
            <input type="hidden" name="start" value="#{position}">
        <div id="board">
          $forall r <- rs
            <div class="row">
              <div class="row-label">#{r}
              $forall c <- cs
                <div class="column">
                  <button name="position" title="#{cconv c}#{r}" value="#{cconv c}#{r}">#{prettyPrintPiece $ getPiece nextBoard (c, r)}
          <div class="row">
            <div class="row-label">
            $forall c <- cs
              <div class="column-label">#{cconv c}
    |]

postHomeR = getHomeR

getNotFoundR = defaultLayout $ do
  setTitle "Not found"
  toWidget [hamlet|<h1>Oof!|]
  [whamlet|
    <p>We couldn't find your page
  |]

main :: IO ()
main = warp 3000 App
