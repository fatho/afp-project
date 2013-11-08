{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR) where

import Import
import Data.Text

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
             setTitle "Guess The Number"
             $(widgetFile "introduction")
