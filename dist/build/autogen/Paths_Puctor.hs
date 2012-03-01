module Paths_Puctor (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/ms/Library/Haskell/ghc-7.0.4/lib/Puctor-0.1/bin"
libdir     = "/Users/ms/Library/Haskell/ghc-7.0.4/lib/Puctor-0.1/lib"
datadir    = "/Users/ms/Library/Haskell/ghc-7.0.4/lib/Puctor-0.1/share"
libexecdir = "/Users/ms/Library/Haskell/ghc-7.0.4/lib/Puctor-0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Puctor_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Puctor_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Puctor_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Puctor_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
