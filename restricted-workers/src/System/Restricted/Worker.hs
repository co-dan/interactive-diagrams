{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Main entry point
module System.Restricted.Worker
    (
      module System.Restricted.Worker.Types
--    , module System.Restricted.Worker.Pool
    , module System.Restricted.Worker.Protocol
    , mkDefaultWorker
    , startWorker
    , killWorker
    , startIOWorker
    ) where

import Prelude                           hiding (mapM_)

import Control.Monad                     (forever)
import Control.Monad.Base                (MonadBase (..))
import Control.Monad.IO.Class            (MonadIO, liftIO)
import Data.Foldable                     (mapM_)
import Data.Typeable                     ()
import Network                           (Socket, accept)
import System.IO                         (Handle)
import System.Posix.User                 (getEffectiveUserID,
                                          setEffectiveUserID)

import System.Restricted.Limits
import System.Restricted.Types
import System.Restricted.Worker.Internal
-- import System.Restricted.Worker.Pool
import System.Restricted.Worker.Protocol
import System.Restricted.Worker.Types

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
startWorker :: (WorkerData w, MonadIO (WMonad w),
                MonadBase (WMonad w) m)
            => String         -- ^ Name
            -> FilePath       -- ^ Socket
            -> Maybe (IO Handle)  -- ^ Where to redirect stdout, stderr
            -> LimitSettings  -- ^ Restrictions
            -> WMonad w (WData w)  -- ^ Pre-forking action
            -> (WData w -> Socket -> IO ())  -- ^ Socket callback
            -> WMonad w (Worker w, RestartWorker m w)
startWorker name sock out set pre cb = do
    let defW = mkDefaultWorker name sock set
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
            w'' `seq` return w''
    w' <- restarter defW
    return (w', liftBase . restarter)


-- | Start a worker of type 'IOWorker'
-- The callback function is called every time a connectino is established
--
-- >>> startIOWorker "test" "/tmp/test.sock" $ \h -> hPutStrLn h "hello, world"
--
startIOWorker :: String              -- ^ Name
              -> LimitSettings       -- ^ Restrictions
              -> FilePath            -- ^ UNIX socket
              -> (Handle -> IO ())   -- ^ Callback
              -> IO (Worker IOWorker, RestartWorker IO IOWorker)
startIOWorker name set sock callb = startWorker name sock out set preFork handle
  where handle () soc = forever $ do
            (hndl, _, _) <- accept soc
            callb hndl
        out     =  Nothing
        preFork =  putStrLn ("Starting worker " ++ show name)
                >> return ()

