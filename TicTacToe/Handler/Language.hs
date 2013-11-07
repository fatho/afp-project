{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Language (getLanguageR) where

import Import
import Data.Text

getLanguageR :: String -> Handler Html
getLanguageR lang = defaultLayout $ do 
                      setLanguage $ pack lang
                      setUltDestReferer
                      redirectUltDest HomeR
