module Paths_lattester (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/staff/iinf/ben/.cabal/bin"
libdir     = "/home/staff/iinf/ben/.cabal/lib/ghc-7.6.3/lattester-0.2"
datadir    = "/home/staff/iinf/ben/.cabal/share/lattester-0.2"
libexecdir = "/home/staff/iinf/ben/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "lattester_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lattester_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lattester_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lattester_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
