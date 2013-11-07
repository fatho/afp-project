module Instances where

import Prelude
import Logic.TicTacToe
import Data.Text (pack, unpack)
import Web.PathPieces

instance PathPiece TicTacToe where
  fromPathPiece = deserializeField . unpack
  toPathPiece   = pack . serializeField

instance PathPiece Player where
  fromPathPiece pp    = case unpack pp of
    "X" -> Just PlayerX
    "O" -> Just PlayerO
    _   -> Nothing
  toPathPiece PlayerX = pack "X"
  toPathPiece PlayerO = pack "O"