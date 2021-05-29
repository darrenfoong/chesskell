{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Board (advanceBoard, getPiece, mkBoard, prettyPrintPiece, promotePawns, readBoard, writeBoard)
import Data.Char
import Data.Maybe
import Data.Text (Text, append, unpack)
import Logic (genMove, isInCheck, isInCheckmate)
import Move (readMove, writeMove)
import Scoring (scoreBoard, scoreBoardTableInner)
import System.Random
import Types (Color (..), swapColor)
import Yesod

data App = App

data State = Start | PendingMoveStart | PendingMoveEnd deriving (Eq, Show)

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
    toWidgetHead [hamlet|<meta name="keywords" content="chess">|]
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

getState :: Maybe Text -> Maybe Text -> State
getState mStart mPosition
  | isNothing mStart && isNothing mPosition = Start
  | isJust mStart && isJust mPosition = PendingMoveStart
  | otherwise = PendingMoveEnd

getHomeR :: HandlerFor App Html
getHomeR = do
  mBoard <- lookupPostParam "board"
  mPosition <- lookupPostParam "position"
  mStart <- lookupPostParam "start"
  mPreviousBlackMove <- lookupPostParam "previousBlackMove"

  let state = getState mStart mPosition

  let mWhiteMove = case state of
        Start -> Nothing
        PendingMoveStart -> do
          start <- mStart
          end <- mPosition
          return (readMove $ unpack $ append start end)
        PendingMoveEnd -> Nothing

  let ePreviousBoard = case state of
        Start -> Right mkBoard
        _ -> case mBoard of
          Nothing -> Left "ERROR: Inconsistent state; expected post parameter \"board\""
          Just boardStr -> readBoard boardStr
  let color = White

  let ePreviousBoardWithWhite =
        promotePawns <$> case mWhiteMove of
          Nothing -> ePreviousBoard
          Just ePreviousWhiteMove -> do
            previousWhiteMove <- ePreviousWhiteMove
            previousBoard <- ePreviousBoard
            advanceBoard previousBoard color previousWhiteMove

  gen <- liftIO newStdGen

  let emBlackMove = case state of
        Start -> Right Nothing
        PendingMoveStart -> do
          previousBoardWithWhite <- ePreviousBoardWithWhite
          let (_, mMove) = genMove scoreBoard gen previousBoardWithWhite $ swapColor color
           in Right mMove
        PendingMoveEnd -> Right Nothing
  let ePreviousBoardWithBlack =
        promotePawns <$> do
          mBlackMove <- emBlackMove
          case mBlackMove of
            Nothing -> ePreviousBoardWithWhite
            Just blackMove -> do
              previousBoardWithWhite <- ePreviousBoardWithWhite
              advanceBoard previousBoardWithWhite (swapColor color) blackMove

  let mBlackMove = case emBlackMove of
        Left _ -> Nothing
        Right mMove -> case mMove of
          Nothing -> case mPreviousBlackMove of
            Nothing -> Nothing
            Just previousBlackMove -> case readMove $ unpack previousBlackMove of
              Left _ -> Nothing
              Right blackMove -> Just blackMove
          Just move -> Just move
  let mErr = either Just (const Nothing) ePreviousBoardWithBlack
  let mNextBoard = case ePreviousBoardWithBlack of
        Left _ -> either (const Nothing) Just ePreviousBoard
        Right previousBoardWithBlack -> Just previousBoardWithBlack

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
      $maybe err <- mErr
        <p>#{err}
      <p>State: #{show state}
      <p>You are White.
      $maybe nextBoard <- mNextBoard
        <p>White: score: #{scoreBoardTableInner nextBoard White}; check: #{show (isInCheck nextBoard White)}; checkmate: #{show (isInCheckmate nextBoard White)}
        <p>Black: score: #{scoreBoardTableInner nextBoard Black}; check: #{show (isInCheck nextBoard Black)}; checkmate: #{show (isInCheckmate nextBoard Black)}
      $if state /= Start
        $maybe blackMove <- mBlackMove
          <p>Black played: #{writeMove blackMove}
      $if state /= PendingMoveEnd
        <p>Please select a piece.
      $else
        $maybe position <- mPosition
          <p>You selected <span>#{position}</span>. Please select where the piece should go.
    |]
    let cs = [1 .. 8]
    let cconv i = chr (i + ord 'a' - 1)
    let rs = reverse [1 .. 8]
    [whamlet|
      <form method="post" action=@{HomeR}>
        $maybe nextBoard <- mNextBoard
          <input type="hidden" name="board" value="#{writeBoard nextBoard}">
        $if state == PendingMoveEnd
          $maybe position <- mPosition
            <input type="hidden" name="start" value="#{position}">
        $maybe blackMove <- mBlackMove
          <input type="hidden" name="previousBlackMove" value="#{writeMove blackMove}">
        $maybe nextBoard <- mNextBoard
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

postHomeR :: HandlerFor App Html
postHomeR = getHomeR

getNotFoundR :: HandlerFor App Html
getNotFoundR = defaultLayout $ do
  setTitle "Not found"
  toWidget [hamlet|<h1>Oof!|]
  [whamlet|
    <p>We couldn't find your page
  |]

main :: IO ()
main = warp 3000 App
