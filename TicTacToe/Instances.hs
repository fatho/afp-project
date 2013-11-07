module Instances where

import Prelude
import Logic.TicTacToe
import Data.Text (pack, unpack)
import Web.PathPieces

instance PathPiece TicTacToe where
  fromPathPiece = deserializeField . unpack
  toPathPiece   = pack . serializeField
