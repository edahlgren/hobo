module Paths_hobogen (
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
version = Version {versionBranch = [0,2,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/erin/hobo/.hsenv_hobo/cabal/bin"
libdir     = "/home/erin/hobo/.hsenv_hobo/cabal/lib/hobogen-0.2.0.0/ghc-7.6.1"
datadir    = "/home/erin/hobo/.hsenv_hobo/cabal/share/hobogen-0.2.0.0"
libexecdir = "/home/erin/hobo/.hsenv_hobo/cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hobogen_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hobogen_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hobogen_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hobogen_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
