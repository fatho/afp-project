module Logic.Encryption where

import Control.Applicative
import Crypto.PubKey.RSA
#ifdef DEPLOY
import Crypto.Random.API
#else
import Crypto.Random
#endif
import System.IO
import qualified Data.ByteString as BS
import qualified Data.Serialize as Bin
import Prelude

loadKey :: FilePath -> IO PrivateKey
loadKey path = do
  withFile path ReadMode $ \handle -> do
    str <- BS.hGetContents handle
    case Bin.decode str of
      Left ex -> error ex 
      Right key -> return key

instance Bin.Serialize PublicKey where
  get = PublicKey <$> Bin.get
                  <*> Bin.get
                  <*> Bin.get
  put = undefined

instance Bin.Serialize PrivateKey where
  get = PrivateKey 
      <$> Bin.get 
      <*> Bin.get
      <*> Bin.get
      <*> Bin.get
      <*> Bin.get
      <*> Bin.get
      <*> Bin.get
  put = undefined



#ifdef DEPLOY
getCPRG :: IO SystemRandom
getCPRG = getSystemRandomGen
#else
getCPRG :: IO SystemRNG
getCPRG = do
  entPool <- createEntropyPool
  return $ cprgCreate entPool
#endif