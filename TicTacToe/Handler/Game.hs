{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Text
import Data.Maybe
import Diagrams.Prelude

import Logic.TicTacToe
import Logic.Rendering
import Logic.MiniMax

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

             let maybeResult = outcome currentGame
             if isJust $ maybeResult
                then do 
                  setSession "gameState" (pack $ serializeGameState newGame)
                  [whamlet| 
                     <a href=@{HomeR}>#{result maybeResult} Go back.
                     <br>
                     <a href=@{GameR}>Play again.
                  |]
                else [whamlet| |]


postGameR :: Handler Html
postGameR = defaultLayout $ do
              Just currentState <- lookupSession "gameState"
              (x,y) <- runInputPost $ (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

              let 
                hit = (sample emptyBoardPicture (p2 (x,-y))) !! 0
                Just currentGame = deserializeGameState $ unpack currentState
                newGame = processClick currentGame hit

              setSession "gameState" (pack $ serializeGameState $ newGame)
              redirect GameR

processClick :: TicTacToe -> Pos -> TicTacToe
processClick game clickedField = if boardAt (board game) clickedField == Nothing && (isNothing $ outcome game)
	                                then 
                                        let game' = move game clickedField in 
                                        if isNothing $ outcome game' 
                                            then makeComputerMove game'
                                            else game'
	                                else game 

makeComputerMove :: TicTacToe -> TicTacToe
makeComputerMove game = if isJust game'
                           then fromJust game'
                           else game
                             where game' = nextDraw game

result :: Maybe (Maybe Player) -> String
result Nothing         = "Game has not yet ended."
result (Just Nothing)  = "Game has ended in a draw."
result (Just (Just x)) = "Game has ended. " ++ show x ++ " has won." 


nextDraw :: TicTacToe -> Maybe TicTacToe
nextDraw
  = fmap (\(Node x _) -> x)
  . selectMaxAB
  . prune 7
  . unfoldTree moves
