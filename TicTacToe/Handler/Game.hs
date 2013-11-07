{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Data.Text
import Data.Maybe
import Diagrams.Prelude

import Logic.TicTacToe
import Logic.Rendering
import Logic.MiniMax

import Debug.Trace

traceId :: Show a => a -> a
traceId x = traceShow x x

getGameR :: Handler Html
getGameR = defaultLayout $ do
  $(widgetFile "introduction")
  setTitle "A strange game."

  Just currentState <- lookupSession "gameState"
  let 
    Just currentGame = deserializeField $ unpack (traceId currentState)
    
  [whamlet|
    <p>
      <embed src=@{FieldR currentGame} type="image/svg+xml" onload="this.getSVGDocument().onclick = function(event){var form = document.createElement('form');form.setAttribute('method','post');form.setAttribute('action','');var hiddenField=document.createElement('input');hiddenField.setAttribute('type','hidden');hiddenField.setAttribute('name','X');hiddenField.setAttribute('value',event.clientX);form.appendChild(hiddenField);var hiddenField=document.createElement('input');hiddenField.setAttribute('type','hidden');hiddenField.setAttribute('name','Y');hiddenField.setAttribute('value',event.clientY);form.appendChild(hiddenField);document.body.appendChild(form);form.submit();};">
  |]

  let info = gameInfo currentGame
  if hasEnded info
     then do 
       setSession "gameState" (pack $ serializeField initialField)
       [whamlet| 
          <a href=@{HomeR}>#{result info} Go back.
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
                Just currentGame = deserializeField $ unpack currentState
                nextGameState = processClick currentGame PlayerX hit

              setSession "gameState" (pack $ serializeField $ nextGameState)
              redirect GameR

processClick :: TicTacToe -> Player -> Pos -> TicTacToe
processClick game human clickedField = if (isNothing $ getField game clickedField) && (isNothing $ winner game)
	                                then 
                                        let game' = setField (traceId game) human clickedField in 
                                        if isNothing $ winner (traceId game')
                                            then makeComputerMove game'
                                            else game'
	                                else game 

makeComputerMove :: TicTacToe -> TicTacToe
makeComputerMove game = maybe game id $ nextDrawO game

result :: GameInfo -> String
result InProgress = "Game is in progress."
result Draw       = "Game has ended with a draw."
result (WonBy x)  = "Game has ended. " ++ show x ++ " has won." 


nextDrawX :: TicTacToe -> Maybe TicTacToe
nextDrawX
  = fmap (\(Node x _) -> x)
  . selectMinAB
  . prune 7
  . unfoldTree moves


nextDrawO :: TicTacToe -> Maybe TicTacToe
nextDrawO
  = fmap (\(Node x _) -> x)
  . selectMaxAB
  . prune 7
  . unfoldTree moves
