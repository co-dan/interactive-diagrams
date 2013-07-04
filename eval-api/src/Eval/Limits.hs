{-# LANGUAGE RecordWildCards #-}
module Eval.Limits where

import Prelude hiding (mapM_)

import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Monoid (mconcat)
import Data.List (intersperse)
import Data.Foldable (mapM_)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Process (nice)
import System.Posix.Types (ProcessID)
import System.Posix.Resource (setResourceLimit)
import System.Linux.SELinux (getCon, setCon, SecurityContext)

import Eval.EvalM
import Eval.EvalError
import Eval.EvalSettings


setLimits :: LimitSettings -> IO ()
setLimits LimitSettings{..} = do
  mapM_ setRLimits rlimits
  nice niceness
  mapM_ setupSELinuxCntx secontext
    

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

-- | Set the security context.
-- To be more precise, it only sets up the type.
-- Example usage:
--        
-- > setupSELinuxCntx "my_restricted_t"

-- SELinx context has the following format
-- user:role:type:level
-- we only modify the type part
setupSELinuxCntx :: SecurityContext -> IO ()
setupSELinuxCntx ty = do
  con <- splitBy (==':') <$> getCon
  when (length con /= 4) $ error ("Bad context: " ++ mconcat con)
  setCon $ mconcat $ intersperse ":" [con !! 0, con !! 1, ty, con !! 3]
  
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ []     = []
splitBy f (x:xs)
  | f x       = splitBy f xs
  | otherwise = s : splitBy f s'
  where (s, s') = break f (x:xs)
