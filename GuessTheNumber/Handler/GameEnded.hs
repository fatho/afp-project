{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.GameEnded (getGameEndedR) where

import Import

getGameEndedR :: Int -> Handler Html
getGameEndedR number = defaultLayout $ do 
			             setNormalTitle
			             $(widgetFile "game-ended")