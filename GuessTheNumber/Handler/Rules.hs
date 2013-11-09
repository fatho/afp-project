{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Handler.Rules (getRulesR, postRulesR) where

import Import
import Handler.NewGame

getRulesR :: Handler Html
getRulesR = renderRulePage (1,100) Nothing

postRulesR :: Handler Html
postRulesR = do
  range <- runInputPost $ (,)
            <$> iopt intField "lower" 
            <*> iopt intField "upper"
  case range of
    (Just lower, Just upper)
      | lower <= upper -> startNewGame (lower, upper)
      | otherwise      -> renderRulePage (lower,upper) $ Just MsgInvalidRange
    (Just lower, _)    -> renderRulePage (lower, 100) $ Just MsgNoInput
    (_, Just upper)    -> renderRulePage (1, upper) $ Just MsgNoInput
    (_,_)              -> renderRulePage (1, 100) $ Just MsgNoInput

renderRulePage :: (Int, Int) -> Maybe AppMessage -> Handler Html
renderRulePage (lower, upper) formError = defaultLayout $ do
  setNormalTitle
  $(widgetFile "rules")