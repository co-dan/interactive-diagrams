{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Eval.Worker.Types where

import Control.Monad (when)
import Control.Exception (IOException, Exception, handle)
import Data.Maybe (isJust, fromJust)
import Data.Typeable
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Process (getProcessStatus)
import System.Posix.Types (ProcessID)

import GHC

import Eval.EvalSettings

-- | A worker restarting function
type RestartWorker m a = Worker a -> m (Worker a)

-- | A datatype representing a worker of type 'a'
data Worker a = Worker
    { -- | Name of the worker
      workerName     :: String
      -- | A filepath to the Unix socket that will be
      -- used for communicating with the worker.
      -- If the file is already present it will be unliked
      -- during the initializatin step
    , workerSocket   :: FilePath
      -- | Security restrictions for the worker
    , workerLimits   :: LimitSettings
      -- | 'Just pid' if the worker's process ID is 'pid',
      -- Nothing' if the worker is not active/initialized
    , workerPid      :: Maybe ProcessID
    } deriving (Show)


data IOWorker
data EvalWorker

class WorkerData w where
  -- | Data that saves after restarts
  type WData w :: *
  -- | Monad in which the worker runs
  type WMonad w :: * -> *

instance WorkerData IOWorker where
  type WData IOWorker = ()
  type WMonad IOWorker = IO
  
instance WorkerData EvalWorker where
  type WData EvalWorker = HscEnv
  type WMonad EvalWorker = IO
    
    
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
processAlive pid = do
  handle (\(e :: IOException) -> return False) $ do
    tc <- getProcessStatus False False pid
    return True

workerAlive :: Worker a -> IO Bool
workerAlive w = do
  case (workerPid w) of
    Nothing  -> return False
    Just pid -> processAlive pid

-- | Kills a worker, take an initialized worker,
-- returns non-initialized one.
killWorker :: Worker a -> IO (Worker a)
killWorker w@Worker{..} = do
  when (initialized w) $ do
    alive <- processAlive (fromJust workerPid)
    when alive $ do
      signalProcess killProcess (fromJust workerPid)
      tc <- getProcessStatus False False (fromJust workerPid)
      case tc of
        Just _  -> return ()
        Nothing -> signalProcess killProcess (fromJust workerPid)
  return (w { workerPid = Nothing })    
  
