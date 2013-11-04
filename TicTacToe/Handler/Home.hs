{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR, postHomeR) where

import Import
import Data.Text

import Logic.TicTacToe
import Logic.Rendering

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
             $(widgetFile "introduction")
             setTitle "A strange game."
             setSession "gameState" (pack $ serializeGameState newGame)
             [whamlet|
               <a href=@{GameR}>Start playing.
             |]
