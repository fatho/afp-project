module Handler.NewGame (getNewGameR, startNewGame) where

import Import
import System.Random
import Logic.State

getNewGameR :: Int -> Int -> Handler Html
getNewGameR = curry startNewGame

-- | Creates the initial game state and redirects to guessing page.
-- Make sure that the range argument is validated before calling this function.
startNewGame :: (Int, Int) -> Handler Html
startNewGame (lowerBound, upperBound) = 
  if lowerBound > upperBound 
    then redirect RulesR -- just to be sure...
    else do
      -- Setup Game
      num   <- liftIO $ getStdRandom $ randomR (lowerBound, upperBound)
      let state = GameState
                    { myNumber = fromIntegral num  
                    , guessHistory = []
                    }
      redirect (GuessR $ encryptGameState state)