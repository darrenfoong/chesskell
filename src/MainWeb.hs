{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Board (getPiece, mkBoard, parseMove)
import Control.Applicative ((<$>), (<*>))
import Data.Char
import Data.Maybe
import Data.Text (Text, pack)
import System.Random
import Types (CPiece (..), Color (..), Piece (..))
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
        PendingMoveStart -> Nothing
        PendingMoveEnd -> do
          start <- mStart
          end <- mPosition
          return (show start ++ show end)

  let gen = getStdGen
  let board = mkBoard

  --  let move = case mPreviousWhiteMove of
  --                Just previousWhiteMove -> parseMove previousWhiteMove
  --                Nothing -> Left "nah"

  let mPreviousBlackMove = Just "e5e5"

  defaultLayout $ do
    setTitle "chesskell"
    [whamlet|
      <p>You are White.
      $if state /= Start
        $maybe previousBlackMove <- mPreviousBlackMove
          <p>Black played: #{previousBlackMove}

      $if state /= PendingMoveEnd
        <p>Please select a piece.
      $else
        $maybe position <- mPosition
          <p>You selected <span>#{position}</span>. Please select where the piece should go.
    |]
    let rs = [1 .. 8]
    let cs = reverse [1 .. 8]
    let rconv = \i -> chr (i + ord 'a' - 1)
    [whamlet|
      <form method="post" action=@{HomeR}>
        $if state == PendingMoveEnd
          $maybe position <- mPosition
            <input type="hidden" name="start" value="#{position}">
        <table>
          $forall r <- rs
            <tr>
              $forall c <- cs
                <td><button name="position" value="#{rconv r}#{c}">#{prettyPrintPiece $ getPiece board (c, r)}
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
