{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}
module Eval.Limits where

import Prelude hiding (mapM_)

import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Monoid (mconcat)
import Data.List (intersperse)
import Data.Foldable (mapM_)
import System.FilePath.Posix ((</>))
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Process (nice, getProcessID)
import System.Posix.Types (ProcessID, UserID, CUid(..))
import System.Posix.User (setUserID, setEffectiveUserID, getRealUserID, getEffectiveUserID)
import System.Posix.Resource (setResourceLimit)
import System.Linux.SELinux (getCon, setCon, SecurityContext)
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error

import Eval.EvalM
import Eval.EvalError
import Eval.EvalSettings
import SignalHandlers

foreign import ccall unsafe "unistd.h chroot"
  c_chroot :: CString -> IO CInt

chroot :: FilePath -> IO ()
chroot fp = do
  eid <- getEffectiveUserID
  setUserID (CUid 0)
  withCString fp $ \c_fp -> do
    throwErrnoIfMinus1 "chroot" (c_chroot c_fp)
    changeWorkingDirectory "/" 
    return ()
  setEffectiveUserID eid

changeUserID :: UserID -> IO ()
changeUserID uid = do
  setUserID (CUid 0) -- need to be root in order to setuid()
  setUserID uid

-- | Add a process to a cgroup
setCGroup :: LimitSettings 
          -> ProcessID      -- ^ The ID of a process to be added to the group
          -> IO ()
setCGroup LimitSettings{..} pid =
  mapM_ (\fp -> writeFile (fp </> "tasks") $ show pid) cgroupPath
               
setLimits :: LimitSettings -> IO ()
setLimits LimitSettings{..} = do
  mapM_ setRLimits rlimits
  mapM_ setupSELinuxCntx secontext
  nice niceness
  mapM_ chroot chrootPath
  mapM_ changeUserID processUid
  restoreHandlers
  where getUserID = (,) <$> getEffectiveUserID <*> getRealUserID
  

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
               , (ResourceFileSize, fileSizeLimit)
               , (ResourceOpenFiles, openFilesLimit)
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

