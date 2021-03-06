{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module System.Restricted.Types
    (
      -- * Limit settings
      LimitSettings(..)
    , RLimits(..)
    , defaultLimits
    ) where


import Data.Default
import Data.Serialize        (Serialize)
import GHC.Generics
import System.Linux.SELinux  (SecurityContext)
import System.Posix.Resource (Resource (..), ResourceLimit (..),
                              ResourceLimits (..))
import System.Posix.Types    (CUid (..), UserID)


-- | Resource limits
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

-- | Datastructure that holds the information about restrictions and
--   limitations for the worker process
data LimitSettings = LimitSettings
    { -- | Maximum time for which the code is allowed to run
      -- (in seconds)
      timeout    :: Int
      -- | Process priority for the 'nice' syscall.
      -- -20 is the highest, 20 is the lowest
    , niceness   :: Int
      -- | Resource limits for the 'setrlimit' syscall
    , rlimits    :: Maybe RLimits
      -- | The directory that the evaluator process will be 'chroot'ed
      -- into. Please note that if chroot is applied, all the pathes
      -- in 'EvalSettings' will be calculated relatively to this
      -- value.
    , chrootPath :: Maybe FilePath
      -- | The UID that will be set after the call to chroot.
    , processUid :: Maybe UserID
      -- | SELinux security context under which the worker
      -- process will be running.
    , secontext  :: Maybe SecurityContext
      -- | A filepath to the 'tasks' file for the desired cgroup.
      --
      -- For example, if I have mounted the @cpu@ controller at
      -- @/cgroups/cpu/@ and I want the evaluator to be running in the
      -- cgroup 'idiaworkers' then the 'cgroupPath' would be
      -- @/cgroups/cpu/idiaworkers@
    , cgroupPath :: Maybe FilePath
    } deriving (Eq, Show, Generic)

deriving instance Generic CUid
instance Serialize CUid
instance Serialize LimitSettings

-- | Default 'LimitSettings'
defaultLimits :: LimitSettings
defaultLimits = LimitSettings
    { timeout    = 3
    , niceness   = 10
    , rlimits    = Nothing
    , chrootPath = Nothing
    , processUid = Nothing
    , secontext  = Nothing -- Just "idia_restricted_t"
    , cgroupPath = Nothing
    }

instance Default LimitSettings where
    def = defaultLimits

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
openFilesLimitSoft = ResourceLimit 20
openFilesLimitHard = ResourceLimit 50
-- TODO: It would be nice to set these to zero, but right now Hint gets around the
-- insecurity of the GHC API by writing stuff out to a file in /tmp, so we need
-- to allow our compiled binary to do file I/O... :( But at least we can still limit
-- how much we write out!
fileSizeLimitSoft = fileSizeLimitHard
fileSizeLimitHard = ResourceLimitUnknown
dataSizeLimitSoft = dataSizeLimitHard
dataSizeLimitHard = ResourceLimit $ 104857600 * 5 -- 100 * 5 mb
-- These should not be identical, to give the XCPU handler time to trigger
cpuTimeLimitSoft = ResourceLimit 4
cpuTimeLimitHard = ResourceLimit 5
coreSizeLimitSoft = coreSizeLimitHard
coreSizeLimitHard = zero

-- convenience
zero = ResourceLimit 0


