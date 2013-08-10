{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Evaluation settings
module Diagrams.Interactive.Eval.EvalSettings
    (
      EvalSettings(..)
    , defaultSettings
    , LimitSettings(..)
    , defaultLimits
    , RLimits(..)
    , ResourceLimits(..)
    , ResourceLimit(..)
    , Resource(..)
    ) where

import Data.Default
import System.IO             (Handle)
import System.Posix.Resource

import GHC.Paths

import System.Restricted.Types

-- | Datastructure holding the settings for the interpreter
data EvalSettings = EvalSettings
    { -- | Path to the directory with Haskell libraries
      libDirPath   :: Maybe FilePath
      -- | A list of pathes to @package.conf.d@ directories
    , pkgDatabases :: [FilePath]
      -- | Path to the directory where temporary files are held
      -- the sockets will be stored there
    , tmpDirPath   :: FilePath
      -- | Verbosity level
      -- Note [Verbosity levels]
      -- ~~~~~~~~~~~~~~~~~~~~~~~
      -- 0 | log errors & warnings only
      -- 1 | minimal verbosity: print "compiling M ... done." for each module.
      -- 2 | equivalent to -dshow-passes
      -- 3 | equivalent to existing "ghc -v"
      -- 4 | "ghc -v -ddump-most"
      -- 5 | "ghc -v -ddump-all"
    , verbLevel    :: Int
      -- File name that will be used for source code.
      -- /Warning: obsolete/
      --, fileName    :: FilePath
      -- | A handle where the output will be redirected to
      -- (to be precise, an action that would run in the worker
      -- environemnt and would return a handle)
    , outHandle    :: Maybe (IO Handle)
      -- | Security restrictions
    , limitSet     :: LimitSettings
      -- | File that has to be preloaded
    , preloadFile  :: FilePath
    } deriving (Eq, Show)

instance Show (IO Handle) where
  show _ = "<IO Handle>"

instance Eq (IO Handle) where
  _ == _ = False

{- | Default value for 'EvalSettings'
@
defaultSettings = EvalSettings
    { tmpDirPath   = "/tmp"
    , libDirPath   = Just libdir
    , pkgDatabases = []
    , verbLevel    = 1
    , outHandle    = Nothing -- return stdout
    , limitSet     = def
    , preloadFile  = "Preload.hs"
    }
@
-}  
defaultSettings :: EvalSettings
defaultSettings = EvalSettings
    { tmpDirPath   = "/tmp"
    , libDirPath   = Just libdir
    , pkgDatabases = []
    , verbLevel    = 1
    , outHandle    = Nothing -- return stdout
    , limitSet     = def
    , preloadFile  = "Preload.hs"
    }

instance Default EvalSettings where
  def = defaultSettings

