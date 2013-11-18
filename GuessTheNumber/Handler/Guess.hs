{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, QuasiQuotes, PackageImports #-}
module Handler.Guess (getGuessR, postGuessR) where

import Import
import Data.Maybe
#ifdef DEPLOY
import Crypto.Random.API
#else
import Crypto.Random
#endif
import Crypto.PubKey.RSA
import Logic.State
import Logic.Encryption

import Debug.Trace

traceId x = traceShow x x

getGuessR :: EncGameState -> Handler Html
getGuessR encryptedState = do 
  defaultLayout $ do
    privKey <- liftIO $ loadKey "config/rsa_key"
    case decryptGameState privKey encryptedState of
      Just (GameState {myNumber = myNum', .. }) -> do
        setNormalTitle
        let
          totalGuesses = length guessHistory
          lastGuess = fmap fromIntegral $ listToMaybe guessHistory :: Maybe Int
          myNumber = fromIntegral myNum' :: Int

        $(widgetFile "guess-form")
      Nothing -> redirect HomeR

someForm :: Form Int
someForm = renderTable $ areq intField "" Nothing

postGuessR :: EncGameState -> Handler Html
postGuessR encryptedState = do 
  formResult <- runInputPost $ iopt intField "guess"
  privKey <- liftIO $ loadKey "config/rsa_key"
  cprg <- liftIO $ getCPRG

  case formResult of
    Just i ->
      defaultLayout $ case decryptGameState privKey encryptedState of
        Nothing  -> redirect HomeR
        Just gs  -> do
          let 
            newState = gs 
                { myNumber = myNumber gs
                , guessHistory = i : guessHistory gs
                }
            targetRoute = if i == myNumber gs then GameEndedR else GuessR

          redirect (targetRoute $ encryptGameState cprg (private_pub privKey) newState)
    _ -> redirect (GuessR encryptedState)