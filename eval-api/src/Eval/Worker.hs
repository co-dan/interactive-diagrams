{-# LANGUAGE DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies, StandaloneDeriving, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, EmptyDataDecls, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeHoles #-}
module Eval.Worker
       (
         module Eval.Worker.EvalCmd,
         -- * The 'Worker' type
         Worker(..), initialized,
         startWorker, killWorker,
         -- ** IOWorker
         IOWorker, startIOWorker,
         -- ** EvalWorker
         EvalWorker, startEvalWorker,
         sendCompileFileRequest, sendEvalStringRequest
       ) where

import Prelude hiding (putStr)
  
import Control.Monad (when, forever, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Control.Concurrent.Async (race)
import Control.Exception (IOException)
import Data.ByteString (hGetContents, hPutStr, hGetLine, putStr)
import qualified Data.ByteString as BS
import Data.Typeable
import Data.Default
import Data.Function (fix)
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>))
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import Data.Serialize (encode, decode, Serialize)
import GHC.IO.Handle (hSetBuffering, BufferMode(..), hFlush)
import Network (listenOn, connectTo, accept, PortID(..), Socket)
import Network.Socket (close)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO (Handle, hClose)
import System.Posix.Files (removeLink)
import System.Posix.Process (forkProcess, getProcessStatus, ProcessStatus(..))
import System.Posix.Signals (signalProcess, killProcess, Handler(..), installHandler, setStoppedChildFlag, processStatusChanged)
import System.Posix.Types (ProcessID)

import DynFlags
import GHC hiding (compileExpr)
import MonadUtils hiding (MonadIO, liftIO)

import Eval hiding (runToHandle)
import Eval.EvalError
import Eval.Helpers
import Eval.Limits
import Eval.EvalSettings (LimitSettings(..), EvalSettings(..))
import Eval.EvalM
import Eval.Worker.EvalCmd
import Eval.Worker.Protocol
import Eval.Worker.Types
import Display


-- | Start a general type of worker
startWorker :: Worker a -- ^ A non-active worker
            -> (Socket -> IO ()) 
            -> IO (Worker a)
startWorker w cb = do
  pid <- forkWorker w cb
  return $ w { workerPid = Just pid }

forkWorker :: Worker a -> (Socket -> IO ()) -> IO ProcessID
forkWorker Worker{..} cb = do
  -- setStoppedChildFlag True
  -- installHandler processStatusChanged Ignore Nothing
  soc <- mkSock workerSocket
  forkProcess $ do
    setStoppedChildFlag False
    -- installHandler processStatusChanged Default Nothing
    -- setLimits workerLimits
    cb soc
    return ()

-- | Start a worker of type 'IOWorker'
startIOWorker :: Worker IOWorker -- ^ A non-active worker
              -> (Handle -> IO ())
              -> IO (Worker IOWorker)
startIOWorker w callb = startWorker w $ \soc -> do
  forever $ do
    (hndl, _, _) <- accept soc
    callb hndl
    

killWorker :: Worker a -> IO ()
killWorker w@Worker{..} = do
  when (initialized w) $ do
    tc <- getProcessStatus False False (fromJust workerPid)
    case tc of
      Just _  -> return ()
      Nothing -> signalProcess killProcess (fromJust workerPid)
  
-- | Starts a specialized worker for running EvalM
-- preloads stuff, etc
startEvalWorker :: String                   -- ^ Name of the worker
                -> EvalSettings             -- ^ Evaluation settings that will be used
                -> IO (Worker EvalWorker)
startEvalWorker name eset = do
  pid <- flip run' eset $ do
    -- liftEvalM $ initGhc _
    dfs <- getSessionDynFlags
    setSessionDynFlags $ dfs { hscTarget = HscInterpreted
                             , ghcLink = LinkInMemory
                             }
    loadFile (preloadFile eset)
    sess <- getSession
    liftIO $ forkWorker worker $ \soc -> do
      forever $ do
        (hndl, _, _) <- accept soc
        act <- evalWorkerAction hndl
        flip run' eset $ do
          setSession sess
          r :: EvalResultWithErrors <- liftEvalM $
                                       runToHandle (runEvalM act eset) hndl
          liftIO $ hFlush hndl
          return r
  return $ worker { workerPid = Just pid }
  where worker = Worker { workerName   = name
                        , workerSocket = tmpDirPath eset </> (name ++ ".sock")
                        , workerLimits = limitSet eset
                        , workerPid    = Nothing
                        }


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

sendEvalRequest :: Worker EvalWorker
                -> EvalCmd
                -> IO (EvalResultWithErrors, Worker EvalWorker)
sendEvalRequest w cmd = do
  hndl <- connectToWorker w
  let pid = fromJust . workerPid $ w
  let timelimit = timeout . workerLimits $ w
  -- r <- performEvalRequest hndl cmd
  r <- race (processTimeout pid timelimit) $ do
    requestResult <- performEvalRequest hndl cmd
    -- tc <- getProcessStatus False False pid
    -- case tc of
    --   Just (Stopped _) -> signalProcess killProcess pid
    --   Nothing          -> signalProcess killProcess pid
    --   _                -> return ()
    return requestResult
  r `seq` hClose hndl
  tc <- getProcessStatus False False pid
  w' <- case tc of
    Nothing -> return w 
    Just _  -> startEvalWorker (workerName w) (def { limitSet = workerLimits w })
  let evres = case r of
        Left _ -> (Left "Process timedout", [])
        Right (Right x) ->  x
        Right (Left str) -> (Left $ "Deserialization error:\n" ++
                             str, [])
  return (evres, w')

-- | Send the 'Worker' a request to compile a file
sendCompileFileRequest :: Worker EvalWorker
                       -> FilePath
                       -> IO (EvalResultWithErrors, Worker EvalWorker)
sendCompileFileRequest w fpath = sendEvalRequest w (CompileFile fpath)
  
-- | Send the 'Worker' a request to compile an expression
sendEvalStringRequest :: Worker EvalWorker
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
                  
