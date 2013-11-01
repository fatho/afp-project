module Paths_paketname (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/meike/.cabal/bin"
libdir     = "/home/meike/.cabal/lib/x86_64-linux-ghc-7.6.3/paketname-0.0.0"
datadir    = "/home/meike/.cabal/share/x86_64-linux-ghc-7.6.3/paketname-0.0.0"
libexecdir = "/home/meike/.cabal/libexec"
sysconfdir = "/home/meike/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "paketname_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "paketname_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "paketname_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "paketname_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "paketname_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
