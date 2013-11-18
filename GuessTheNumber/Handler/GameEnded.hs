{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.GameEnded (getGameEndedR) where

import Import
import Logic.State
import Logic.Encryption

getGameEndedR :: EncGameState -> Handler Html
getGameEndedR encState = defaultLayout $ do 
  privKey <- liftIO $ loadKey "config/rsa_key"
  case decryptGameState privKey encState of
    Nothing -> redirect HomeR
    Just (GameState {myNumber = myNum', guessHistory = gh}) -> do
      setNormalTitle
      let 
        myNumber = fromIntegral myNum' :: Int
        guessHistory = map fromIntegral gh :: [Int]
        totalGuesses = length guessHistory
      $(widgetFile "game-ended")