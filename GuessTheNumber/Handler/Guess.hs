{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Guess (getGuessR, postGuessR) where

import Import
import Data.Maybe
import Logic.State

import Debug.Trace

traceId x = traceShow x x

getGuessR :: EncGameState -> Handler Html
getGuessR encryptedState = do 
  defaultLayout $ do
    case decryptGameState encryptedState of
      Just (GameState {..}) -> do
        setNormalTitle
        let
          totalGuesses = length guessHistory
          lastGuess = listToMaybe guessHistory

        $(widgetFile "guess-form")
      Nothing -> redirect HomeR

someForm :: Form Int
someForm = renderTable $ areq intField "" Nothing

postGuessR :: EncGameState -> Handler Html
postGuessR encryptedState = do 
  formResult <- runInputPost $ iopt intField "guess"
  case formResult of
    Just i ->
      defaultLayout $ case decryptGameState encryptedState of
        Nothing  -> redirect HomeR
        Just gs  -> do
          newState <- liftIO $ newSalt $ gs 
                { myNumber = myNumber gs
                , guessHistory = i : guessHistory gs
                }
          let targetRoute = if i == myNumber gs then GameEndedR else GuessR
          redirect (targetRoute $ encryptGameState newState)
    _ -> redirect (GuessR encryptedState)