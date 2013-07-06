{-# LANGUAGE DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies, StandaloneDeriving, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, EmptyDataDecls, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeHoles #-}
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

import Prelude hiding (putStr)
  
import Control.Applicative ((<$>))
import Control.Concurrent.Async (race)
import Control.Exception (IOException, handle)
import Control.Monad (when, forever)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Default
import Data.IORef (newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Serialize (Serialize)
import Data.Typeable ()
import Network (listenOn, connectTo, accept, PortID(..), Socket)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO (Handle, hClose)
import System.Posix.Files (removeLink)
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Signals (signalProcess, killProcess, Handler(..), installHandler, setStoppedChildFlag, processStatusChanged)
import System.Posix.Types (ProcessID)

import DynFlags
import GHC hiding (compileExpr)
import MonadUtils hiding (MonadIO, liftIO)

import Display
import Eval hiding (runToHandle)
import Eval.EvalError
import Eval.EvalM
import Eval.EvalSettings (LimitSettings(..), EvalSettings(..))
import Eval.Helpers
import Eval.Limits
import Eval.Worker.EvalCmd
import Eval.Worker.Protocol
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
        pre  = flip run' eset $ do
          dfs <- getSessionDynFlags
          setSessionDynFlags $ dfs { hscTarget = HscInterpreted
                                   , ghcLink = LinkInMemory
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

-- | Runs a Ghc monad code and outputs the result to a handle  
runToHandle :: (Serialize a, Show a)
            => Ghc a -> Handle -> Ghc (Either String a, [EvalError])
runToHandle act hndl = do
  ref <- liftIO $ newIORef []
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { log_action = logHandler ref }
  dr :: Either String a <- handleException act
  errors :: [EvalError] <- liftIO $ readIORef ref
  liftIO $ sendData hndl (dr, errors)
  return (dr, errors)


performEvalRequest :: Handle -> EvalCmd -> IO (DecodeResult EvalResultWithErrors)
performEvalRequest hndl cmd = do
  sendData hndl cmd
  (Right <$> getData hndl) `gcatch` \(e :: ProtocolException) ->
    return (Left (show e))

-- | Send the worker a request to evaluate something.
-- If the process dies because of hitting limits, it is restarted
sendEvalRequest :: (Worker EvalWorker, RestartWorker IO EvalWorker)
                -> EvalCmd
                -> IO (EvalResultWithErrors, Worker EvalWorker)
sendEvalRequest (w, restart) cmd = do
  hndl <- connectToWorker w
  let pid = fromJust . workerPid $ w
  let timelimit = timeout . workerLimits $ w
  -- r <- performEvalRequest hndl cmd
  r <- race (processTimeout pid timelimit) $ do
    requestResult <- performEvalRequest hndl cmd
    return requestResult
  r `seq` hClose hndl
  alive <- processAlive pid
  w' <- case alive of
    True  -> return w 
    -- False -> startEvalWorker (workerName w) (def { limitSet = workerLimits w })
    False -> restart w
  let evres = case r of
        Left _ -> (Left "Process timedout", [])
        Right (Right x) ->  x
        Right (Left str) -> (Left $ "Deserialization error:\n" ++
                             str, [])
  return (evres, w')


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
                          
------------------------------------------------------------

connectToWorker :: Worker a -> IO Handle
connectToWorker Worker{..} = connectTo "localhost" (UnixSocket workerSocket)
  
mkSock :: FilePath -> IO Socket
mkSock sf = do
  exists <- doesFileExist sf
  when exists $ removeLink sf
  listenOn (UnixSocket sf)
                  
