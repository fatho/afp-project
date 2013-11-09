{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Guess (getGuessR, postGuessR) where

import Import

getGuessR :: String -> Handler Html
getGuessR encryptedState = 
  do 
    (formWidget, formEnctype) <- generateFormPost someForm
    defaultLayout $ do
                 setNormalTitle
                 $(widgetFile "guess-form")

someForm :: Form Int
someForm = renderTable $ areq intField "guess" Nothing

postGuessR :: String -> Handler Html
postGuessR encryptedState = 
  do ((FormSuccess i,_), _) <- runFormPost someForm
     defaultLayout $ do
                  msg <- translateMessage (MsgLastGuess i)
                  setMessage $ toHtml msg
                  redirect (GuessR encryptedState)