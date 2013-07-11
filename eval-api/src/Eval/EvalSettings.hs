{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module Eval.EvalSettings
    (
     EvalSettings(..), defaultSettings,
     LimitSettings(..), defaultLimits,
     RLimits(..),
     ResourceLimits(..), ResourceLimit(..), Resource(..),
     SecurityContext
    ) where

import Data.Default
import Data.Serialize (Serialize)
import System.Posix.Resource (ResourceLimit(..),
                              ResourceLimits(..),
                              Resource(..))
import System.Posix.Types (UserID, CUid(..))
import System.Linux.SELinux (SecurityContext)  
import GHC.Paths
import GHC.Generics

data RLimits = RLimits
    { coreFileSizeLimit :: ResourceLimits
    , cpuTimeLimit      :: ResourceLimits
    , dataSizeLimit     :: ResourceLimits
    , fileSizeLimit     :: ResourceLimits
    , openFilesLimit    :: ResourceLimits
    , stackSizeLimit    :: ResourceLimits
    , totalMemoryLimit  :: ResourceLimits
    } deriving (Eq, Show, Generic)

deriving instance Show ResourceLimits
deriving instance Show ResourceLimit
deriving instance Show Resource
deriving instance Generic ResourceLimit
deriving instance Generic ResourceLimits
instance Serialize ResourceLimit
instance Serialize ResourceLimits
instance Serialize RLimits
         
         
-- | Datastructure holding the settings for the interpreter  
data EvalSettings = EvalSettings
    { -- | Path to the directory with Haskell libraries
      libDirPath   :: Maybe FilePath
      -- | A list of pathes to @package.conf.d@ directories
    , pkgDatabases :: [FilePath]
      -- | Path to the directory where temporary files are held
      -- the sockets will be stored there
    , tmpDirPath   :: FilePath
      -- File name that will be used for source code.
      -- /Warning: obsolete/
      --, fileName    :: FilePath
      -- | Security restrictions
    , limitSet     :: LimitSettings
      -- | File that has to be preloaded
    , preloadFile  :: FilePath
    } deriving (Eq, Show)


data LimitSettings = LimitSettings
    { -- | Maximum time for which the code is allowed to run
      -- (in seconds)
      timeout     :: Int
      -- | Process priority for the 'nice' syscall.
      -- -20 is the highest, 20 is the lowest
    , niceness    :: Int
      -- | Resource limits for the 'setrlimit' syscall
    , rlimits     :: Maybe RLimits
      -- | The directory that the evaluator process will be 'chroot'ed
      -- into. Please note that if chroot is applied, all the pathes
      -- in 'EvalSettings' will be calculated relatively to this
      -- value.
    , chrootPath  :: Maybe FilePath
      -- | The UID that will be set after the call to chroot.
    , processUid  :: Maybe UserID
      -- | SELinux security context under which the worker 
      -- process will be running.
    , secontext   :: Maybe SecurityContext
    } deriving (Eq, Show, Generic)

deriving instance Generic CUid    
instance Serialize CUid
instance Serialize LimitSettings               

defaultSettings :: EvalSettings
defaultSettings = EvalSettings
    { tmpDirPath   = "/tmp"  
    , libDirPath   = Just libdir
    , pkgDatabases = []
    , limitSet     = def
    , preloadFile  = "Preload.hs"
    }

defaultLimits :: LimitSettings
defaultLimits = LimitSettings
    { timeout    = 3
    , niceness   = 10
    , rlimits    = Nothing
    , chrootPath = Nothing
    , processUid = Nothing
    , secontext  = Nothing -- Just "idia_restricted_t"
    }

instance Default LimitSettings where
  def = defaultLimits
    
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
dataSizeLimitHard = ResourceLimit $ 104857600 * 5 -- 100 * 5 mb
-- These should not be identical, to give the XCPU handler time to trigger
cpuTimeLimitSoft = ResourceLimit 4
cpuTimeLimitHard = ResourceLimit 5
coreSizeLimitSoft = coreSizeLimitHard
coreSizeLimitHard = zero

-- convenience
zero = ResourceLimit 0
  
