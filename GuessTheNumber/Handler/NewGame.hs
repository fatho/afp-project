module Handler.NewGame (getNewGameR, startNewGame) where

import Import
import Control.Monad
import System.Random

getNewGameR :: Int -> Int -> Handler Html
getNewGameR = curry startNewGame

startNewGame :: (Int, Int) -> Handler Html
startNewGame (lowerBound, upperBound) = do
  -- verify input
  when (lowerBound > upperBound) $ fail "invalid bounds"
  -- Setup Game
  myNumber <- liftIO $ getStdRandom $ randomR (lowerBound, upperBound)
  -- TODO: encrypt that number
  let encrypted = show myNumber
  redirect (GuessR encrypted)