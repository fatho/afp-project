{-# LANGUAGE RecordWildCards, NamedFieldPuns, PackageImports #-}
module Logic.State
  ( GameState (..)
  , EncGameState (..)
  , encryptGameState
  , decryptGameState
  ) where

import Prelude
import Control.Applicative
#ifdef DEPLOY
import Crypto.Random.API
#else
import Crypto.Random
#endif
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import Data.Serialize as Bin
import Data.ByteString as BS
import Data.ByteString.Char8 as BS8
import Data.ByteString.Base16 as B16
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.Random
import Web.PathPieces
import Data.Int

data GameState = GameState 
                  { myNumber :: Int16
                  , guessHistory :: [Int16]
                  } deriving (Show)

newtype EncGameState = EncGameState { gameStateBS :: BS.ByteString } deriving (Eq)

encryptGameState :: CPRG g => g -> PublicKey -> GameState -> EncGameState
encryptGameState g pk = EncGameState . discardErrors . encrypt g pk . Bin.encode where
  discardErrors (Left _, _) = BS.empty
  discardErrors (Right x, _) = x

decryptGameState :: PrivateKey -> EncGameState -> Maybe GameState
decryptGameState pk encst = either (const Nothing) Just $ (showLeft $ decrypt Nothing pk (gameStateBS encst)) >>= Bin.decode where
  showLeft (Left x) = Left (show x)
  showLeft (Right x) = Right x

instance Show EncGameState where
  show = BS8.unpack . encStateToBase64

instance Read EncGameState where
  readsPrec _ str = case base64ToEncState $ BS8.pack str of
    Nothing -> []
    Just x  -> [(x,"")]

instance Bin.Serialize GameState where
  put (GameState {..}) = Bin.put myNumber >> Bin.put guessHistory
  get = GameState <$> Bin.get <*> Bin.get

instance PathPiece EncGameState where
  fromPathPiece = base64ToEncState . encodeUtf8
  toPathPiece   = decodeUtf8 . encStateToBase64

base64ToEncState :: BS.ByteString -> Maybe EncGameState
base64ToEncState = fmap EncGameState . toMaybe . B16.decode where
  toMaybe (xs,xxs)
    | BS8.null xxs = Just xs
    | otherwise = Nothing

encStateToBase64 :: EncGameState -> BS.ByteString
encStateToBase64 = B16.encode . gameStateBS