{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | Main entry point of the library
module Eval where

import Prelude hiding (writeFile, readFile, mapM_)

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, liftM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..))
import Data.ByteString (writeFile, readFile, hGetContents, hPutStr)
import Data.Foldable (mapM_)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import Data.Serialize (encode, decode, Serialize)
import Network (listenOn, connectTo, accept, socketPort, PortID(..), Socket(..))
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO (Handle, hClose)
import System.Posix.Files (removeLink)
import System.Posix.Process (nice, forkProcess, getProcessStatus)
import System.Posix.Signals (signalProcess, killProcess)

import DynFlags
import Exception
import GHC
import MonadUtils hiding (MonadIO, liftIO)
import Outputable
import Panic


import Display
import Eval.EvalError
import Eval.EvalM
import Eval.EvalSettings
import Eval.Helpers
import Eval.Limits
import SignalHandlers

import Debug.Trace hiding (traceM)
-- | Debug function  
traceM :: Monad m => String -> m ()
traceM s = trace s $ return ()
  
-- | Evaluation result together with a list of errors/warnings
type EvalResultWithErrors = (EvalResult, [EvalError])

-- | A queue for evaluator                            
newtype EvalQueue =
  EvalQueue (Chan (EvalM DisplayResult, MVar EvalResultWithErrors))

-- | Runs an EvalM monad and returns either a result, or an error message
run :: EvalM a -> EvalSettings -> IO (Either String a)
run m set = do
  ref <- newIORef []
  r <- handleException $ run' (liftEvalM (initGhc ref) >> m) set
  logMsg <- unlines . map show <$> readIORef ref
  case r of
    Left s -> return $ Left $ s ++ "\n" ++ logMsg
    _ -> return r

run' :: EvalM a -> EvalSettings -> IO a
run' m set = runGhc (libDirPath set) (runEvalM m set)

-- | Inits the GHC API, sets the mode and the log handler         
initGhc :: IORef [EvalError] -> Ghc ()
initGhc ref = do
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { hscTarget = HscInterpreted
                           , ghcLink = LinkInMemory
                           , log_action = logHandler ref 
                           }
  return ()


-- | A log handler for GHC API. Saves the errors and warnings in an 'IORef'
-- LogAction == DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logHandler :: IORef [EvalError] -> LogAction
logHandler ref dflags severity srcSpan style msg =
  case srcSpan of
    RealSrcSpan sp -> do
      modifyIORef' ref (++ [err sp])
    UnhelpfulSpan _ -> return ()
  where err sp = EvalError severity msg' (srcPos sp)
        cntx = initSDocContext dflags style
        msg' = show (runSDoc msg cntx)

        
-- | Exception handler for GHC API.
-- Catches all exceptions and restores handlers.        
handleException :: (ExceptionMonad m, MonadIO m)
                   => m a -> m (Either String a)
handleException m =
  ghandle (\(ex :: SomeException) -> return (Left (showException ex))) $
  handleGhcException (\ge -> return (Left (showGhcException ge ""))) $
  flip gfinally (liftIO restoreHandlers) $
  liftM Right m
  


-- | Spawn a new evaluator
prepareEvalQueue :: EvalSettings             -- ^ Settings to use for compilation
                 -> IO (EvalQueue, ThreadId) -- ^ A queue to send requests to and the resulting thread ID           
prepareEvalQueue set = do                    
  chan <- EvalQueue <$> newChan
  tid <- forkIO $ do
    run (do
      loadFile "Preload.hs"
      sess <- getSession
      -- this is causing problems (memory leaks?):
      -- forever $ liftIO $ forkIO $ run' (handleQueue chan sess) set) set
      forever $ handleQueue chan sess) set
    return ()
  return (chan, tid)
  

-- | Send a request to the evaluator
sendEvaluator :: EvalQueue           -- ^ Queue created by 'prepareEvalQueue'
              -> EvalM DisplayResult -- ^ 'EvalM' code that renders to 'DisplayResult'
              -> IO EvalResultWithErrors
sendEvaluator (EvalQueue chan) act = do
  mv <- newEmptyMVar
  writeChan chan (act, mv)
  takeMVar mv
  
  
-- | Function that handles requests from the 'EvalQueue'
handleQueue :: EvalQueue -> HscEnv -> EvalM ()  
handleQueue (EvalQueue chan) sess = do
  (act', resultMVar) <- liftIO $ readChan chan
  r <- runWithLimits act' sess
  liftIO $ putMVar resultMVar r
  return ()

-- | Sets up initial limits, prepares the socket and the monadic action
-- and runs it in a restricted environment
runWithLimits :: EvalM DisplayResult -> HscEnv -> EvalM EvalResultWithErrors
runWithLimits act' sess = do
  (set@EvalSettings{..}) <- ask
  let act = do
        liftIO $ nice (niceness limitSet)
        runEvalM act' set

  liftIO $ do
    exists <- doesFileExist (tmpDirPath </> fileName)
    when exists $ removeLink (tmpDirPath </> fileName)
  soc <- liftIO $ listenOn (UnixSocket (tmpDirPath </> fileName))
  liftEvalM $ execTimeLimit act set soc sess
    
  
-- | Compiles a source code file using 'compileFile' and writes
-- the result to a file
runToFile :: Serialize a => Ghc a -> FilePath -> Ghc ()
runToFile act f = do
  ref <- liftIO $ newIORef []
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { log_action = logHandler ref }
  dr <- handleException act
  errors <- liftIO $ readIORef ref
  liftIO $ writeFile (f ++ ".res") (encode (dr,errors))
  return ()

-- | Runs a Ghc monad code and outputs the result to a handle  
runToHandle :: (Serialize a, Show a)
            => Ghc a -> Handle -> Ghc (Either String a, [EvalError])
runToHandle act hndl = do
  ref <- liftIO $ newIORef []
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { log_action = logHandler ref }
  dr :: Either String a <- handleException act
  errors :: [EvalError] <- liftIO $ readIORef ref
  liftIO $ hPutStr hndl (encode (dr,errors))
  liftIO $ hPutStr hndl "\n"
  return (dr, errors)
  
  
-- | Result of the deserialization
type DecodeResult a = Either String a

-- | Executes an action using time restrictions
execTimeLimit :: Ghc DisplayResult -- ^ Action to be executed
              -> EvalSettings      -- ^ Settings with time limit
              -> Socket            -- ^ A socket that should be used for communication
              -> HscEnv            -- ^ Session in which the action will be executed
              -> Ghc (EvalResult, [EvalError])
execTimeLimit act set soc sess = do
  pid <- liftIO . forkProcess . flip run' set $ do
    liftIO $ do
      mapM_ setRLimits (rlimits (limitSet set))
      case secontext (limitSet set) of
        Just cntx -> setupSELinuxCntx cntx
        Nothing   -> return ()
    setSession sess
    hndl <- liftIO $ connectTo "localhost" =<< socketPort soc
    liftEvalM $ runToHandle act hndl
    return ()
  (hndl, _, _) <- liftIO $ accept soc
  r <- liftIO $ race (processTimeout pid (timeout (limitSet set))) $ do
      tc <- getProcessStatus True False pid
      case tc of
        Just exitSt -> do
          (r :: DecodeResult EvalResultWithErrors) <- decode <$> hGetContents hndl
          case r of
            Right eres -> return eres
            Left str -> return (Left $ "Deserialization error:\n" ++
                                str ++ "\nExit status was: " ++ show exitSt, [])
        Nothing -> do
          signalProcess killProcess pid
          return (Left (show TooLong), [])
  either return return r


