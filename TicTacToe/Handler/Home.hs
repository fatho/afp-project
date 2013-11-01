{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Home (getHomeR, postHomeR) where

import Import
import Data.Text
import Diagrams.Prelude

import Handler.Circle

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  $(widgetFile "home")
  [whamlet|
   <p>und noch etwas hamlet-HTML im QuasiQuoter in Home.hs, eine Aufzählung:
     <ul>
       <li>erster Eintrag
       <li>zweiter Eintrag
   <p>
     Ein Bild gefällig?<br/>
     <embed src=@{CircleR} type="image/svg+xml" onload="this.getSVGDocument().onclick = function(event){var form = document.createElement('form');form.setAttribute('method','post');form.setAttribute('action','');var hiddenField=document.createElement('input');hiddenField.setAttribute('type','hidden');hiddenField.setAttribute('name','X');hiddenField.setAttribute('value',event.clientX);form.appendChild(hiddenField);var hiddenField=document.createElement('input');hiddenField.setAttribute('type','hidden');hiddenField.setAttribute('name','Y');hiddenField.setAttribute('value',event.clientY);form.appendChild(hiddenField);document.body.appendChild(form);form.submit();};">
          |]
  setMessage "This message was set on the previous page. It goes away if you reload this page."
  [whamlet|
   <p>
     <a href=@{AnotherR}>Going to another page?
          |]

postHomeR :: Handler Html
postHomeR = defaultLayout $ do
  (x,y) <- runInputPost $ (,) <$> ireq doubleField "X" <*> ireq doubleField "Y"
  let hit = sample picture (p2 (x,-y))
  setMessage $ toHtml (pack $ show hit)
  redirect HomeR
