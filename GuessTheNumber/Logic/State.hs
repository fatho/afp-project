{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Logic.State
  ( GameState (..)
  , EncGameState (..)
  , encryptGameState
  , decryptGameState
  , newSalt
  ) where

import Prelude
import Control.Applicative
import Data.Serialize as Bin
import Data.ByteString as BS
import Data.ByteString.Char8 as BS8
import Data.ByteString.Base64 as B64
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.Random
import Web.PathPieces

data GameState = GameState 
                  { myNumber :: Int
                  , guessHistory :: [Int]
                  , salt :: Int
                  } deriving (Show)

newtype EncGameState = EncGameState { gameStateBS :: BS.ByteString } deriving (Eq)

encryptGameState :: GameState -> EncGameState
encryptGameState = EncGameState . Bin.encode 

decryptGameState :: EncGameState -> Maybe GameState
decryptGameState = either (const Nothing) Just . Bin.decode . gameStateBS

newSalt :: GameState -> IO GameState
newSalt gs = getStdRandom random >>= \salt -> return gs {salt}

instance Show EncGameState where
  show = BS8.unpack . encStateToBase64

instance Read EncGameState where
  readsPrec _ str = case base64ToEncState $ BS8.pack str of
    Nothing -> []
    Just x  -> [(x,"")]

instance Bin.Serialize GameState where
  put (GameState {..}) = Bin.put myNumber >> Bin.put guessHistory >> Bin.put salt
  get = GameState <$> Bin.get <*> Bin.get <*> Bin.get

instance PathPiece EncGameState where
  fromPathPiece = base64ToEncState . encodeUtf8
  toPathPiece   = decodeUtf8 . encStateToBase64

base64ToEncState :: BS.ByteString -> Maybe EncGameState
base64ToEncState = fmap EncGameState . either (const Nothing) Just . B64.decode

encStateToBase64 :: EncGameState -> BS.ByteString
encStateToBase64 = B64.encode . gameStateBS