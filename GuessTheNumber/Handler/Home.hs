{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR) where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
             setNormalTitle
             $(widgetFile "home")