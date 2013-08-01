{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Worker
    (
      module Worker.Types
    , module Worker.Pool
    , module Worker.Protocol
    , mkDefaultWorker
    , startWorker
    , startIOWorker
    ) where

import Worker.Internal
import Worker.Internal.Limits
import Worker.Pool
import Worker.Protocol
import Worker.Types

import Prelude                hiding (mapM_)

import Control.Applicative    ((<$>), (<*>))
import Control.Exception      (catch, throwIO)
import Control.Monad          (forever, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Foldable          (mapM_)
import Data.Typeable          ()
import Network                (Socket, accept)
import Network.Socket         (close)
import System.Directory
import System.FilePath.Posix  ((</>))
import System.IO              (Handle, stdout)
import System.IO.Error        (isAlreadyExistsError, isPermissionError)
import System.Mem.Weak        (addFinalizer)
import System.Posix.IO        (closeFd, dupTo, handleToFd)
import System.Posix.Process   (forkProcess, getProcessID)
import System.Posix.Signals   (Handler (..), installHandler,
                               processStatusChanged, setStoppedChildFlag)
import System.Posix.Types     (Fd (..), ProcessID)
import System.Posix.User      (getEffectiveUserID, getRealUserID,
                               setEffectiveUserID)


-- | Create an uninitialized worker
mkDefaultWorker :: String -> FilePath -> LimitSettings -> Worker a
mkDefaultWorker name sock set = Worker
    { workerName    = name
    , workerSocket  = sock
    , workerLimits  = set -- def { secontext = Nothing }
    , workerPid     = Nothing
    }


{-|
  Start a general type of worker.

  The pre-forking action is a monadic action that will be run prior to
  calling 'forkWorker'. It might be some intialization code, running the
  DB query, anything you want. The resuling 'WData' will be passed to
  the callback.
-}
startWorker :: (WorkerData w, MonadIO (WMonad w))
            => String         --  ^ Name
            -> FilePath       --  ^ Socket
            -> Maybe (IO Handle)         --  ^ Where to redirect stdout, stderr
            -> LimitSettings  --  ^ Restrictions
            -> WMonad w (WData w) --  ^ Pre-forking action
            -> (WData w -> Socket -> IO ()) -- ^ Socket callback
            -> WMonad w (Worker w, RestartWorker (WMonad w) w)
startWorker name sock out set pre cb = do
    let w = mkDefaultWorker name sock set
    let restarter !w = do
            w' <- liftIO $ killWorker w
            oldId <- liftIO $ getEffectiveUserID
            liftIO $ mapM_ setEffectiveUserID (processUid set)
            -- this is necessary so that the control socket is accessible by
            -- non-root processes, probably a hack
            dat <- pre
            pid <- liftIO $ forkWorker w' out (cb dat)
            liftIO $ setEffectiveUserID oldId
            liftIO $ setCGroup set pid
            let w'' = w' { workerPid = Just pid }
            return w''
            w'' `seq` return w''
    w' <- restarter w
    return (w', restarter)

-- | Start a worker of type 'IOWorker'
-- The callback function is called every time a connectino is established
--
-- >>> startIOWorker "test" "/tmp/test.sock" $ \h -> hPutStrLn h "hello, world"
--
startIOWorker :: String              -- ^ Name
              -> FilePath            -- ^ UNIX socket
              -> (Handle -> IO ())   -- ^ Callback
              -> IO (Worker IOWorker, RestartWorker IO IOWorker)
startIOWorker name sock callb = startWorker name sock out defSet preFork handle
  where handle () soc = forever $ do
          (hndl, _, _) <- accept soc
          callb hndl
        defSet = def { secontext = Nothing }
        out    = Nothing
        preFork =  putStrLn ("Starting worker " ++ show name)
                >> return ()

