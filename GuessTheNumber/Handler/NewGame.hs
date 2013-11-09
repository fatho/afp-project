module Handler.NewGame (getNewGameR, startNewGame) where

import Import
import Control.Monad
import System.Random
import Logic.State

getNewGameR :: Int -> Int -> Handler Html
getNewGameR = curry startNewGame

-- | Creates the initial game state and redirects to guessing page.
-- Make sure that the range argument is validated before calling this function.
startNewGame :: (Int, Int) -> Handler Html
startNewGame (lowerBound, upperBound) = do
  -- Setup Game
  num   <- liftIO $ getStdRandom $ randomR (lowerBound, upperBound)
  state <- liftIO $ newSalt $ GameState
                { myNumber = num  
                , lastGuess = Nothing
                , totalGuesses = 0
                , salt = 0
                }
  redirect (GuessR $ encryptGameState state)