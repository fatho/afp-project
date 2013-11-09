{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Guess (getGuessR, postGuessR) where

import Import
import Data.Text hiding (map)
import Control.Arrow

getGuessR :: String -> Handler Html
getGuessR encryptedState = 
  do 
    (formWidget, formEnctype) <- generateFormPost someForm
    defaultLayout $ do
                 setTitle "Guess The Number"
                 $(widgetFile "guess-form")

someForm :: Form Int
someForm = renderTable $ areq intField "guess" Nothing

postGuessR :: String -> Handler Html
postGuessR encryptedState = 
  do ((FormSuccess i,_), _) <- runFormPost someForm
     defaultLayout $ do
                  setMessage . toHtml . pack $ "You guessed number " ++ show i
                  redirect (GuessR encryptedState)