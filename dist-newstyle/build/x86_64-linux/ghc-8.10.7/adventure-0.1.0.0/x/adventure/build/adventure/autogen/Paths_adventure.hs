{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_adventure (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mc431/.cabal/bin"
libdir     = "/home/mc431/.cabal/lib/x86_64-linux-ghc-8.10.7/adventure-0.1.0.0-inplace-adventure"
dynlibdir  = "/home/mc431/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/mc431/.cabal/share/x86_64-linux-ghc-8.10.7/adventure-0.1.0.0"
libexecdir = "/home/mc431/.cabal/libexec/x86_64-linux-ghc-8.10.7/adventure-0.1.0.0"
sysconfdir = "/home/mc431/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "adventure_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "adventure_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "adventure_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "adventure_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "adventure_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "adventure_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
