{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Eval.Worker.Types where

import Network (Socket)
import Control.Exception (IOException, Exception)
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable
import Data.Maybe (isJust)
import System.Posix.Types (ProcessID)

import GHC

import Eval.EvalSettings

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
    } 


data IOWorker
data EvalWorker

class WorkerData w where
  -- | Data that saves after restarts
  type WData w :: *
  type WMonad w :: * -> *
  -- -- | A hook that is run prior/after forking the process                  
  -- forkHook :: Worker w                 -- ^ Worker (inactive) that will be forked
  --          -> IO ProcessID             -- ^ Forking action
  --          -> IO (ProcessID, WData w)  -- ^ The resulting PID and data

instance WorkerData IOWorker where
  type WData IOWorker = ()
  type WMonad IOWorker = IO
  -- forkHook Worker{..} forkCall = do
  --   putStrLn ("Starting worker " ++ show workerName)
  --   pid <- forkCall
  --   return (pid, ())
  
instance WorkerData EvalWorker where
  type WData EvalWorker = HscEnv
  type WMonad EvalWorker = IO
  -- forkHook Worker{..} forkCall = flip run' 
    
    
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
