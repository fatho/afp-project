{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.GameEnded (getGameEndedR) where

import Import
import Logic.State

getGameEndedR :: EncGameState -> Handler Html
getGameEndedR encState = defaultLayout $ do 
  case decryptGameState encState of
    Nothing -> redirect HomeR
    Just (GameState {..}) -> do
      setNormalTitle
      let totalGuesses = length guessHistory
      $(widgetFile "game-ended")