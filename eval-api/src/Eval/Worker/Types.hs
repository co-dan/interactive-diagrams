{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
module Eval.Worker.Types where

import Network (Socket)
import Control.Exception (IOException, Exception)
import Data.Typeable
import Data.Maybe (isJust)
import System.Posix.Types (ProcessID)


import Eval.EvalSettings

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
