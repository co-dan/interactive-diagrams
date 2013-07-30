{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
module Worker.Internal.Limits where

import Prelude                hiding (mapM_)

import Control.Applicative    ((<$>))
import Control.Monad          (when)
import Data.Foldable          (mapM_)
import Data.List              (intersperse)
import Data.Monoid            (mconcat)
import Foreign.C
import Foreign.C.Types
import System.FilePath.Posix  ((</>))
import System.Linux.SELinux   (SecurityContext, getCon, setCon)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Process   (nice)
import System.Posix.Resource  (setResourceLimit)
import System.Posix.Resource  (Resource (..))
import System.Posix.Types     (CUid (..), ProcessID, UserID)
import System.Posix.User      (getEffectiveUserID, setEffectiveUserID,
                               setUserID)

import SignalHandlers
import Worker.Types

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

