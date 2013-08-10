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
module System.Restricted.Worker.Types
    (
      -- * Workers
      Worker(..)
    , RestartWorker
    , WorkerData(..)
    , IOWorker
      -- * Other types
    , ProtocolException(..)
      -- * Helper functions
    , initialized
    ) where

import Control.Exception       (Exception, IOException)
import Data.Maybe              (isJust)
import Data.Serialize          (Serialize)
import Data.Typeable
import GHC.Generics
import System.Posix.Types      (CPid (..), ProcessID)

import System.Restricted.Types

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


