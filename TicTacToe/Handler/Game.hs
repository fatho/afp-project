{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Text
import Diagrams.Prelude

import Logic.TicTacToe
import Logic.Rendering

getGameR :: Handler Html
getGameR = defaultLayout $ do
             $(widgetFile "introduction")
             setTitle "A strange game."
             Just currentState <- lookupSession "gameState"

             let 
               Just currentGame = deserializeGameState $ unpack currentState

             [whamlet|
               <p>
                 <embed src=@{FieldR (board currentGame)} type="image/svg+xml" onload="this.getSVGDocument().onclick = function(event){var form = document.createElement('form');form.setAttribute('method','post');form.setAttribute('action','');var hiddenField=document.createElement('input');hiddenField.setAttribute('type','hidden');hiddenField.setAttribute('name','X');hiddenField.setAttribute('value',event.clientX);form.appendChild(hiddenField);var hiddenField=document.createElement('input');hiddenField.setAttribute('type','hidden');hiddenField.setAttribute('name','Y');hiddenField.setAttribute('value',event.clientY);form.appendChild(hiddenField);document.body.appendChild(form);form.submit();};">
             |]

postGameR :: Handler Html
postGameR = defaultLayout $ do
              Just currentState <- lookupSession "gameState"
              (x,y) <- runInputPost $ (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

              let 
                hit = (sample emptyBoardPicture (p2 (x,-y))) !! 0
                Just currentGame = deserializeGameState $ unpack currentState

              if boardAt (board currentGame) hit == Nothing 
              	then setSession "gameState" (pack $ serializeGameState $ move currentGame hit) 
              	else setSession "gameState" (pack $ serializeGameState currentGame)
              redirect GameR