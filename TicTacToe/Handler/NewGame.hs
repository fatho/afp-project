{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.NewGame (getNewGameR) where

import Import
import Data.Text

import Logic.TicTacToe
import Logic.Rendering

getNewGameR :: Player -> Handler Html
getNewGameR humanPlayer = defaultLayout $ do
             $(widgetFile "introduction")
             setTitle "A strange game."
             setSession "gameState" (pack $ serializeField initialField)
             setSession "human" (pack $ show humanPlayer)
             redirect GameR
