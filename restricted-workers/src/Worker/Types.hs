{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-|
  Worker can be in one of three states

    [Uninitialized] Uninitialized worker is a worker that has a name,
    a socket, possibly 'WData' but has not been forker

    [Initialized] Initialized worker has an associated forker process.

    [Active] A worker is active if it's initialized and it's being used
    a client. Active/inactive workers are managed by a 'WorkersPool'.
-}
module Worker.Types
    (
      -- * Limit settings
      LimitSettings(..)
    , RLimits(..)
    , defaultLimits
      -- * Workers
    , Worker(..)
    , RestartWorker
    , WorkerData(..)
    , IOWorker
      -- * Other types
    , ProtocolException(..)
      -- * Helper functions
    , initialized
    , workerAlive
    , processAlive
    ) where

import Control.Exception     (Exception, IOException, handle)
import Data.Default
import Data.Maybe            (isJust)
import Data.Serialize        (Serialize)
import Data.Typeable
import GHC.Generics
#if !NO_SELINUX
import System.Linux.SELinux  (SecurityContext)
#endif
import System.Posix.Process  (getProcessStatus)
import System.Posix.Resource (Resource (..), ResourceLimit (..),
                              ResourceLimits (..))
import System.Posix.Types    (CUid (..), UserID)
import System.Posix.Types    (CPid (..), ProcessID)

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
#if !NO_SELINUX      
      -- | SELinux security context under which the worker
      -- process will be running.
    , secontext  :: Maybe SecurityContext
#endif
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
#if !NO_SELINUX                   
    , secontext  = Nothing
#endif
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

------------------------------------------------------------

-- | A worker restarting function
type RestartWorker m a = Worker a -> m (Worker a)

-- | A datatype representing a worker of type 'a'
data Worker a = Worker
    { -- | Name of the worker
      workerName   :: String
      -- | A filepath to the Unix socket that will be
      -- used for communicating with the worker.
      -- If the file is already present it will be unliked
      -- during the initializatin step
    , workerSocket :: FilePath
      -- | Security restrictions for the worker
    , workerLimits :: LimitSettings
      -- | 'Just pid' if the worker's process ID is 'pid',
      -- Nothing' if the worker is not active/initialized
    , workerPid    :: Maybe ProcessID
    } deriving (Show, Eq, Typeable, Generic)

deriving instance Generic CPid
instance Serialize CPid
instance Serialize (Worker a)

-- | Types of data attached to a worker.
-- This might be a configuration file, a size of the packet, session data, etc.
class WorkerData w where
    -- | Data that saves after restarts
    type WData w :: *
    -- | Monad in which the worker runs
    type WMonad w :: * -> *

{- | A simple type of worker that executes IO actions

The definition of the 'WorkerData' instance for IOWorker looks like this:

@
  instance WorkerData IOWorker where
      type WData IOWorker = ()
      type WMonad IOWorker = IO
@
-}
data IOWorker

instance WorkerData IOWorker where
    type WData IOWorker = ()
    type WMonad IOWorker = IO

-- | Check whether the worker is initialized
initialized :: Worker a -> Bool
initialized = isJust . workerPid

-- | An exception type used by 'Eval.Worker.Protocol'
data ProtocolException =
    -- | There has been an error during the conversion step
    ConversionException String
    -- | There has been an error while using the handler
    | HandleException IOException
    deriving (Typeable, Show)

instance Exception ProtocolException

-----------------------

-- | Checks whether the process is alive
-- /hacky/
processAlive :: ProcessID -> IO Bool
processAlive pid = handle (\(_ :: IOException) -> return False) $ do
    _ <- getProcessStatus False False pid
    return True

-- | Checks whether the worker is alive
workerAlive :: Worker a -> IO Bool
workerAlive w = do
    case (workerPid w) of
        Nothing  -> return False
        Just pid -> processAlive pid


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

