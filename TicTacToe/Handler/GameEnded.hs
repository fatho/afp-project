{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.GameEnded (getGameEndedR) where

import Import
import Data.Text

import Logic.TicTacToe
import Logic.Rendering

import Handler.Field
import Handler.Session

import Debug.Trace

traceId :: Show a => a -> a
traceId x = traceShow x x

data GameEndedStates = HumanWon | ComputerWon | NobodyWon

getGameEndedR :: Handler Html
getGameEndedR = defaultLayout $ do
  $(widgetFile "introduction")
  setTitle "A strange game."

  Just currentGame <- readGameFromSession
  Just humanPlayer <- getHumanPlayer

  let info = gameInfo currentGame
  if not $ hasEnded info 
    then redirect GameR
    else do
      let 
        gameResult = 
          case gameInfo currentGame of
            WonBy winner -> if winner == humanPlayer
              then HumanWon
              else ComputerWon
            Draw         -> NobodyWon
            _            -> error "impossible"
      $(widgetFile "game-ended")
      fieldWidget currentGame False
      $(widgetFile "game-controls")