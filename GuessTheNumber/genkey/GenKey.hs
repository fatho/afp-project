{-# LANGUAGE PackageImports, RecordWildCards #-}
import "crypto-random" Crypto.Random
import Crypto.PubKey.RSA
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.Serialize as Bin
import System.IO

main :: IO ()
main = do
  entPool <- createEntropyPool
  args <- getArgs
  let
    cprg = cprgCreate entPool :: SystemRNG
    ((pubkey, privkey), _) = generate cprg 128 65537

    str = Bin.encode privkey

  print (BS.length str)

  withFile (head (args ++ ["rsa_key"])) WriteMode $ \handle -> do
    BS.hPutStr handle str
    hFlush handle

instance Bin.Serialize PublicKey where
  put (PublicKey {..}) = Bin.put public_size 
                      >> Bin.put public_n
                      >> Bin.put public_e
  get = undefined

instance Bin.Serialize PrivateKey where
  put (PrivateKey {..}) = 
                         Bin.put private_pub
                      >> Bin.put private_d 
                      >> Bin.put private_p
                      >> Bin.put private_q
                      >> Bin.put private_dP
                      >> Bin.put private_dQ
                      >> Bin.put private_qinv
  get = undefined