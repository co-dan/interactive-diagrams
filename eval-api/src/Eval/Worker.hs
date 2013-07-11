{-# LANGUAGE DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies, StandaloneDeriving, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, EmptyDataDecls, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Worker can be in one of three states

    [Uninitialized] Uninitialized worker is a worker that has a name,
    a socket, possibly 'WData' but has not been forker

    [Initialized] Initialized worker has an associated forker process.

    [Active] A worker is active if it's initialized and it's being used
    a client. Active/inactive workers are managed by a 'WorkersPool'.
-}

module Eval.Worker
       (
         module Eval.Worker.EvalCmd,
         -- * The 'Worker' type
         Worker(..), initialized, RestartWorker,
         startWorker, killWorker,
         -- ** IOWorker
         IOWorker, startIOWorker,
         -- ** EvalWorker
         EvalWorker, startEvalWorker,
         sendCompileFileRequest, sendEvalStringRequest
       ) where

import Prelude hiding (putStr, mapM_)
  
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Default
import Data.Typeable ()
import Data.Foldable (mapM_)
import Network (accept, Socket)
import System.FilePath.Posix ((</>))
import System.IO (Handle)
import System.Posix.Process (forkProcess)
import System.Posix.Signals (Handler(..), installHandler, setStoppedChildFlag, processStatusChanged)
import System.Posix.Types (ProcessID)
import System.Posix.User (getRealUserID, setEffectiveUserID)

import DynFlags
import GHC hiding (compileExpr)

import Display
import Eval hiding (runToHandle)
import Eval.EvalM
import Eval.EvalSettings (LimitSettings(..), EvalSettings(..))
import Eval.Helpers
import Eval.Limits
import Eval.Worker.EvalCmd
import Eval.Worker.Protocol
import Eval.Worker.Internal
import Eval.Worker.Types

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
            -> LimitSettings  --  ^ Restrictions
            -> WMonad w (WData w) --  ^ Pre-forking action
            -> (WData w -> Socket -> IO ()) -- ^ Socket callback
            -> WMonad w (Worker w, RestartWorker (WMonad w) w) 
startWorker name sock set pre cb = do 
  let w = mkDefaultWorker name sock set
  let restarter w = do
        w' <- liftIO $ killWorker w
        dat <- pre
        pid <- liftIO $ forkWorker w' (cb dat)
        return $ w' { workerPid = Just pid }
  w' <- restarter w
  return (w', restarter)

forkWorker :: Worker a -> (Socket -> IO ()) -> IO ProcessID
forkWorker Worker{..} cb = do
  setStoppedChildFlag True
  installHandler processStatusChanged Ignore Nothing
  soc <- mkSock workerSocket
  forkProcess $ do
    setStoppedChildFlag False
    installHandler processStatusChanged Default Nothing
    setLimits workerLimits
    cb soc
    return ()

-- | Start a worker of type 'IOWorker'
-- The callback function is called every time a connectino is established
-- 
-- >>> startIOWorker "test" "/tmp/test.sock" $ \h -> hPutStrLn h "hello, world"
--   
startIOWorker :: String              -- ^ Name
              -> FilePath            -- ^ UNIX socket
              -> (Handle -> IO ())   -- ^ Callback
              -> IO (Worker IOWorker, RestartWorker IO IOWorker)
startIOWorker name sock callb = startWorker name sock defSet preFork handle
  where handle () soc = forever $ do
          (hndl, _, _) <- accept soc
          callb hndl
        defSet = def { secontext = Nothing }
        preFork =  putStrLn ("Starting worker " ++ show name)
                >> return ()
        
  
-- | Starts a specialized worker for running EvalM
-- preloads stuff, etc
startEvalWorker :: String                   -- ^ Name of the worker
                -> EvalSettings             -- ^ Evaluation settings that will be used
                -> IO (Worker EvalWorker, RestartWorker IO EvalWorker)
startEvalWorker name eset = startWorker name sock set pre callback
  where sock = tmpDirPath eset </> (name ++ ".sock")
        set  = limitSet eset
        uid  = processUid set
        pre  = flip run' eset $ do
          liftIO $ mapM_ setEffectiveUserID uid -- this is necessary so that the control socket is accessible by
          -- non-root processes, probably a hack
          addPkgDbs (pkgDatabases eset)
          traceM . ("getRealUserID "++) . show =<< liftIO (getRealUserID)
          dfs <- getSessionDynFlags
          setSessionDynFlags $ dfs { hscTarget = HscInterpreted
                                   , ghcLink = LinkInMemory
                                   , verbosity = 3
                                   }
          loadFile (preloadFile eset)
          getSession
        callback sess soc = forever $ do
          (hndl, _, _) <- accept soc
          act <- evalWorkerAction hndl
          flip run' eset $ do
            setSession sess
            r :: EvalResultWithErrors <- liftEvalM $
                                         runToHandle (runEvalM act eset) hndl
            return r


-- | Read data from a handle and convert it to 'EvalM' action        
evalWorkerAction :: Handle -> IO (EvalM DisplayResult)
evalWorkerAction hndl = do
  (cmd :: EvalCmd) <- getData hndl
                      -- `gcatch` \(e :: ProtocolException) ->
                      -- return (Left (show e))
  return (evalCmdToEvalM cmd)

-- | Send the 'Worker' a request to compile a file
sendCompileFileRequest :: (Worker EvalWorker, RestartWorker IO EvalWorker)
                       -> FilePath
                       -> IO (EvalResultWithErrors, Worker EvalWorker)
sendCompileFileRequest w fpath = sendEvalRequest w (CompileFile fpath)
  
-- | Send the 'Worker' a request to compile an expression
sendEvalStringRequest :: (Worker EvalWorker, RestartWorker IO EvalWorker)
                      -> String
                      -> IO (EvalResultWithErrors, Worker EvalWorker)
sendEvalStringRequest w str = sendEvalRequest w (EvalString str)

