{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative ((<$>), (<*>))
import Data.Char
import Data.Text (Text, pack)
import Yesod
import Yesod.Form

data App = App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
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
  let rs = map (\i -> chr (i + ord 'a' - 1)) [1 .. 8]
  let cs = reverse [1 .. 8]
  defaultLayout $ do
    setTitle "chesskell"
    [whamlet|
      <table>
        $forall r <- rs
          <tr>
          $forall c <- cs
            <td><input type="submit" value="#{r}#{c}">
    |]

getNotFoundR = defaultLayout $ do
  setTitle "Not found"
  toWidget [hamlet|<h1>Oof!|]
  toWidget
    [hamlet|
    <p>We couldn't find your page
    |]

main :: IO ()
main = warp 3000 App
