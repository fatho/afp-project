{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Rules (getRulesR, postRulesR) where

import Import
import Handler.NewGame

getRulesR :: Handler Html
getRulesR = do
  (formWidget, formEnctype) <- generateFormPost ruleForm
  defaultLayout $ do 
    setTitle "Guess The Number"
    $(widgetFile "rules")

postRulesR :: Handler Html
postRulesR = do
  ((result, formWidget), formEnctype) <- runFormPost ruleForm
  case result of
      FormSuccess range -> startNewGame range
      _ -> defaultLayout $ do
            $(widgetFile "rules")

ruleForm :: Form (Int, Int)
ruleForm = renderDivs $ (,)
              <$> areq intField "lower" (Just 1)
              <*> areq intField "upper" (Just 100)