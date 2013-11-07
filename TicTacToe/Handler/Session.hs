{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Session
  ( saveGameToSession
  , readGameFromSession
  , getHumanPlayer
  ) where

import Import
import Data.Text

import Logic.TicTacToe

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

saveGameToSession :: MonadHandler m => TicTacToe -> m ()
saveGameToSession = setSession "gameState" . pack . serializeField 

readGameFromSession :: MonadHandler m => m (Maybe TicTacToe)
readGameFromSession = do
  sess <- lookupSession "gameState"
  return $ case sess of
    Just state -> deserializeField $ unpack state
    Nothing    -> Nothing

getHumanPlayer :: MonadHandler m => m (Maybe Player)
getHumanPlayer = do
  sess <- lookupSession "human"
  return $ case sess of
    Just state -> readMaybe $ unpack state
    Nothing           -> Nothing
