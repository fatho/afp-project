module Logic.Helpers
  ( translateMessage
  , setNormalTitle
  , encryptNumber
  , decryptNumber
  ) where

import Prelude
import Yesod.Core
import Foundation
import Data.Text

translateMessage :: MonadHandler m => AppMessage -> m Text
translateMessage msg = do
  langs <- languages
  return $ renderMessage (undefined :: App) langs msg

setNormalTitle :: Widget 
setNormalTitle = do 
                   msg <- translateMessage MsgGameName
                   setTitle $ toHtml msg


--TODO: Impelment this correctly.
encryptNumber :: Int -> String
encryptNumber = show

decryptNumber :: String -> Int
decryptNumber = read