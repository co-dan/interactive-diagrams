{-# LANGUAGE RecordWildCards #-}
module Eval.Limits where

import Control.Concurrent (threadDelay)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Types (ProcessID)
import System.Posix.Resource (setResourceLimit)
import System.Linux.SELinux (setCon, SecurityContext)

import Eval.EvalM
import Eval.EvalError
import Eval.EvalSettings

-- | Waits for a certain period of time (3 seconds)
-- and then kills the process
processTimeout :: ProcessID -- ^ ID of a process to be killed
               -> Int -- ^ Time limit (in seconds)
               -> IO (EvalResult, [EvalError])
processTimeout pid lim = do
  threadDelay (lim * 1000000)
  -- putStrLn "Timed out, killing process"
  signalProcess killProcess pid
  return (Left (show TooLong), [])


-- | Set rlimits using setrlimit syscall
setRLimits :: RLimits -> IO ()
setRLimits RLimits{..} = mapM_ (uncurry setResourceLimit) lims
  where lims = [ (ResourceCoreFileSize, coreFileSizeLimit)
               , (ResourceCPUTime, cpuTimeLimit)
               , (ResourceDataSize, dataSizeLimit)
               -- , (ResourceFileSize, fileSizeLimit)
               -- , (ResourceOpenFiles, openFilesLimit)
               -- , (ResourceStackSize, stackSizeLimit)
               , (ResourceTotalMemory, totalMemoryLimit) ]


setupSELinuxCntx :: SecurityContext -> IO ()
setupSELinuxCntx = setCon
