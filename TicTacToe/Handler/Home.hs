{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR, postHomeR) where

import Import
import Data.Text
import Diagrams.Prelude

import Logic.TicTacToe
import Logic.Rendering

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  $(widgetFile "introduction")
  [whamlet|
   <p>
     <embed src=@{FieldR emptyBoard} type="image/svg+xml" onload="this.getSVGDocument().onclick = function(event){var form = document.createElement('form');form.setAttribute('method','post');form.setAttribute('action','');var hiddenField=document.createElement('input');hiddenField.setAttribute('type','hidden');hiddenField.setAttribute('name','X');hiddenField.setAttribute('value',event.clientX);form.appendChild(hiddenField);var hiddenField=document.createElement('input');hiddenField.setAttribute('type','hidden');hiddenField.setAttribute('name','Y');hiddenField.setAttribute('value',event.clientY);form.appendChild(hiddenField);document.body.appendChild(form);form.submit();};">
  |]

postHomeR :: Handler Html
postHomeR = defaultLayout $ do
  (x,y) <- runInputPost $ (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"
  let hit = sample emptyBoardPicture (p2 (x,-y))
  setMessage $ toHtml (pack $ show hit)
  redirect HomeR
