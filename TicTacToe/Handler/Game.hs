{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Game (getGameR, postGameR) where

import Import
import Control.Monad
import Data.Text
import Data.Maybe
import Diagrams.Prelude

import Logic.TicTacToe
import Logic.Rendering
import Logic.MiniMax

import Handler.Field
import Handler.Session

import Debug.Trace

traceId :: Show a => a -> a
traceId x = traceShow x x

getGameR :: Handler Html
getGameR = defaultLayout $ do
  $(widgetFile "introduction")
  setTitle "A strange game."

  Just currentGame <- readGameFromSession
  Just humanPlayer <- getHumanPlayer

  let 
    nextGame = if player currentGame /= humanPlayer
                then makeComputerMove currentGame (switch humanPlayer)
                else currentGame
  saveGameToSession nextGame
  if hasEnded $ gameInfo nextGame
    -- Der Computer hat mit seinem Zug gewonnen
    then redirect GameEndedR 
    else renderGameInProgress nextGame
  $(widgetFile "game-inprogress")

postGameR :: Handler Html
postGameR = defaultLayout $ do
              Just currentGame <- readGameFromSession
              Just humanPlayer <- getHumanPlayer

              (x,y) <- runInputPost $ (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"

              let 
                hit = (sample emptyBoardPicture (p2 (x,-y))) !! 0
                nextGameState = processClick currentGame humanPlayer hit

              setSession "gameState" (pack $ serializeField $ nextGameState)
              redirect GameR

renderGameInProgress :: TicTacToe -> Widget
renderGameInProgress field = fieldWidget field True

processClick :: TicTacToe -> Player -> Pos -> TicTacToe
processClick game human clickedField = if (isNothing $ getField game clickedField) && (isNothing $ winner game)
	                                then setField game human clickedField
	                                else game

makeComputerMove :: TicTacToe -> Player -> TicTacToe
makeComputerMove game comp = maybe game id $ nextDraw game where
  nextDraw = case comp of 
    PlayerX -> nextDrawX
    PlayerO -> nextDrawO

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
