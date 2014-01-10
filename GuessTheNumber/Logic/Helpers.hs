module Logic.Helpers
  ( translateMessage
  , setNormalTitle
  ) where

import Prelude
import Yesod.Core
import Foundation
import Data.Text

-- | Translates a message of the current application to the current target language.
translateMessage :: MonadHandler m => AppMessage -> m Text
translateMessage msg = do
  langs <- languages
  return $ renderMessage (undefined :: App) langs msg

-- | Set a default html title.
setNormalTitle :: Widget 
setNormalTitle = translateMessage MsgGameName >>= setTitle . toHtml
