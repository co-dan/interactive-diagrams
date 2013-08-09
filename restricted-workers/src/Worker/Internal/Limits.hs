{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
-- | The implementation of security restrictions
module Worker.Internal.Limits
    (
      -- * Apply restrictions
      setLimits
      -- * Individual limits
    , setRLimits
    , chroot
    , changeUserID
    , setCGroup
#if !NO_SELINUX      
    , setupSELinuxCntx
#endif
    ) where

import Prelude                hiding (mapM_)

import Control.Applicative    ((<$>))
import Control.Monad          (when)
import Data.Foldable          (mapM_)
import Data.List              (intersperse)
import Data.Monoid            (mconcat)
import Foreign.C
import Foreign.C.Types
import System.FilePath.Posix  ((</>))
#if !NO_SELINUX    
import System.Linux.SELinux   (SecurityContext, getCon, setCon)
#endif    
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

-- | Set the chroot jail                 
chroot :: FilePath -> IO ()
chroot fp = do
    eid <- getEffectiveUserID
    setUserID (CUid 0)
    withCString fp $ \c_fp -> do
        _ <- throwErrnoIfMinus1 "chroot" (c_chroot c_fp)
        changeWorkingDirectory "/"
        return ()
    setEffectiveUserID eid

-- | Change the uid of the current process    
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

#if !NO_SELINUX        
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
    when (length con < 4) $ error ("Bad context: " ++ mconcat (intersperse ":" con))
    setCon $ mconcat $ intersperse ":" [con !! 0, con !! 1, ty, con !! 3]

-- | @splitBy (==x)@ is an inverse of @'intersperse' [x]@    
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ []     = []
splitBy f (x:xs)
  | f x       = splitBy f xs
  | otherwise = s : splitBy f s'
  where (s, s') = break f (x:xs)
#endif
        
-- | Apply the 'LimitSettings'        
setLimits :: LimitSettings -> IO ()
setLimits LimitSettings{..} = do
    mapM_ setRLimits rlimits
#if !NO_SELINUX
    mapM_ setupSELinuxCntx secontext
#endif    
    nice niceness
    mapM_ chroot chrootPath
    mapM_ changeUserID processUid
    restoreHandlers
