module Instances where

import Prelude
import Logic.TicTacToe
import Data.Text (pack, unpack)
import Web.PathPieces

instance PathPiece Board where
  fromPathPiece = deserializeBoard . unpack
  toPathPiece   = pack . serializeBoard