{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR) where

import Import
import Data.Text

import Logic.TicTacToe
import Logic.Rendering

import Handler.Session

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
             setTitle "A strange game."
             saveGameToSession initialField
             $(widgetFile "introduction")
             $(widgetFile "game-controls")
             $(widgetFile "language-controls")
