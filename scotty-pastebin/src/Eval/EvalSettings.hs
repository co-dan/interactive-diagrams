{-# LANGUAGE StandaloneDeriving #-}
module Eval.EvalSettings
    (
     EvalSettings(..), defaultSettings,
     RLimits(..),
     ResourceLimits(..), ResourceLimit(..), Resource(..)
    ) where

import Data.Default
import System.Posix.Resource (ResourceLimit(..),
                              ResourceLimits(..),
                              Resource(..))

data RLimits = RLimits
    { coreFileSizeLimit :: ResourceLimits
    , cpuTimeLimit      :: ResourceLimits
    , dataSizeLimit     :: ResourceLimits
    , fileSizeLimit     :: ResourceLimits
    , openFilesLimit    :: ResourceLimits
    , stackSizeLimit    :: ResourceLimits
    , totalMemoryLimit  :: ResourceLimits
    } deriving (Show)

deriving instance Show ResourceLimits
deriving instance Show ResourceLimit
deriving instance Show Resource

-- | Datastructure holding the settings for the interpreter  
data EvalSettings = EvalSettings
    { -- | Path to the directory where temporary files are held
      tmpDirPath  :: FilePath
      -- | File name that will be used for source code.
      -- The result will be written to '<filename>.res'
    , fileName    :: FilePath
      -- | Maximum time for which the code is allowed to run
      -- (in seconds)
    , timeout     :: Int
      -- | Process priority for the 'nice' syscall.
      -- -20 is the highest, 20 is the lowest
    , niceness    :: Int
      -- | Resource limits for the 'setrlimit' syscall
    , rlimits     :: RLimits
    }

    
defaultSettings :: EvalSettings
defaultSettings = EvalSettings
    { tmpDirPath = "/tmp"
    , fileName   = "test.hs"
    , timeout    = 3
    , niceness   = 10
    , rlimits    = def
    }

instance Default EvalSettings where
  def = defaultSettings

instance Default RLimits where
  def = RLimits
        { coreFileSizeLimit = mkLimits (coreSizeLimitSoft, coreSizeLimitHard)
        , cpuTimeLimit      = mkLimits (cpuTimeLimitSoft, cpuTimeLimitHard)
        , dataSizeLimit     = mkLimits (dataSizeLimitSoft, dataSizeLimitHard)
        , fileSizeLimit     = mkLimits (fileSizeLimitSoft, fileSizeLimitHard)
        , openFilesLimit    = mkLimits (openFilesLimitSoft, openFilesLimitHard)
        , stackSizeLimit    = mkLimits (stackSizeLimitSoft, stackSizeLimitHard)
        , totalMemoryLimit  = mkLimits (totalMemoryLimitSoft, totalMemoryLimitHard)
        }


mkLimits :: (ResourceLimit, ResourceLimit) -> ResourceLimits
mkLimits = uncurry ResourceLimits

--- This snippet is taken from the mueval package
-- (c) Gwern Branwen


-- | Set all the available rlimits.
--   These values have been determined through trial-and-error
stackSizeLimitSoft, stackSizeLimitHard, totalMemoryLimitSoft, totalMemoryLimitHard,
 dataSizeLimitSoft, openFilesLimitSoft, openFilesLimitHard, fileSizeLimitSoft, fileSizeLimitHard,
 dataSizeLimitHard, cpuTimeLimitSoft, cpuTimeLimitHard, coreSizeLimitSoft, coreSizeLimitHard, zero :: ResourceLimit
totalMemoryLimitSoft = dataSizeLimitSoft
totalMemoryLimitHard = dataSizeLimitHard
-- These limits seem to be useless?
stackSizeLimitSoft = ResourceLimitUnknown
stackSizeLimitHard = ResourceLimitUnknown
-- We allow a few files to be opened, such as package.conf, because they are necessary. This
-- doesn't seem to be security problem because it'll be opened at the module
-- stage, before code ever evaluates. I hope.
openFilesLimitSoft = openFilesLimitHard
openFilesLimitHard = ResourceLimit 7
-- TODO: It would be nice to set these to zero, but right now Hint gets around the
-- insecurity of the GHC API by writing stuff out to a file in /tmp, so we need
-- to allow our compiled binary to do file I/O... :( But at least we can still limit
-- how much we write out!
fileSizeLimitSoft = fileSizeLimitHard
fileSizeLimitHard = ResourceLimit 10800
dataSizeLimitSoft = dataSizeLimitHard
dataSizeLimitHard = ResourceLimit $ 1 * 10^(6::Int)
-- These should not be identical, to give the XCPU handler time to trigger
cpuTimeLimitSoft = ResourceLimit 4
cpuTimeLimitHard = ResourceLimit 5
coreSizeLimitSoft = coreSizeLimitHard
coreSizeLimitHard = zero

-- convenience
zero = ResourceLimit 0
  
