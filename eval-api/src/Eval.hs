{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Eval where

import Prelude hiding (writeFile, readFile, mapM_)

import Control.Monad (forever, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..))
import Data.IORef (IORef, newIORef, modifyIORef',
                   readIORef)
import Data.Foldable (mapM_)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.ByteString (writeFile, readFile)
import System.FilePath.Posix ((</>))
import System.Posix.Process (nice, forkProcess, getProcessStatus)
import System.Posix.Signals (signalProcess, killProcess)
import Control.Concurrent.Async (race)
import Data.Serialize (encode, decode, Serialize)  

import GHC
import DynFlags
import MonadUtils hiding (MonadIO, liftIO)
import Outputable
import Exception
import Panic


import Display
import SignalHandlers
import Eval.EvalError
import Eval.EvalSettings
import Eval.EvalM
import Eval.Helpers
import Eval.Limits

import Debug.Trace
traceM :: Monad m => String -> m ()
traceM s = trace s $ return ()
  

type EvalResultWithErrors = (EvalResult, [EvalError])
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
                           -- , log_action = logHandler ref }
                           }
  return ()


-- | A log handler for GHC API. Saves the errors and warnings in an @IORef@
-- LogAction == DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logHandler :: IORef [EvalError] -> LogAction
logHandler ref dflags severity srcSpan style msg =
  case srcSpan of
    RealSrcSpan sp -> do
      modifyIORef' ref (++ [err sp])
      errors <- readIORef ref
      print errors
    UnhelpfulSpan _ -> return ()
  -- case severity of
  --   SevError ->   modifyIORef' ref (++ [printDoc])
  --   SevFatal ->   modifyIORef' ref (++ [printDoc])
  --   _ -> return ()
  where err sp = EvalError severity msg' (srcPos sp)
        cntx = initSDocContext dflags style
        msg' = show (runSDoc msg cntx)
        -- locMsg = mkLocMessage severity srcSpan msg
        -- printDoc = show (runSDoc locMsg cntx)


-- | Exception handler for GHC API. Catches all exceptions
-- and restores handlers.        
handleException :: (ExceptionMonad m, MonadIO m)
                   => m a -> m (Either String a)
handleException m =
  ghandle (\(ex :: SomeException) -> return (Left (showException ex))) $
  handleGhcException (\ge -> return (Left (showGhcException ge ""))) $
  flip gfinally (liftIO restoreHandlers) $
  liftM Right m
  


-- | Spawn a new evaluator
prepareEvalQueue :: EvalSettings -> IO (EvalQueue, ThreadId)
prepareEvalQueue set = do
  chan <- EvalQueue <$> newChan
  tid <- forkIO $ do
    run (do
      loadFile "Preload.hs"
      sess <- getSession
      forever $ handleQueue chan sess) set
    return ()
  return (chan, tid)
  

sendEvaluator :: EvalQueue -> EvalM DisplayResult -> IO (EvalResult, [EvalError])
sendEvaluator (EvalQueue chan) act = do
  mv <- newEmptyMVar
  writeChan chan (act, mv)
  takeMVar mv
  
  
-- | Function that handles requests from the @EvalQueue@
handleQueue :: EvalQueue -> HscEnv -> EvalM ()  
handleQueue (EvalQueue chan) sess = do
  (act', resultMVar) <- liftIO $ readChan chan
  -- r <- loadFile fpath sess
  r <- runWithLimits act' sess
  liftIO $ putMVar resultMVar r
  return ()


runWithLimits :: EvalM DisplayResult -> HscEnv -> EvalM EvalResultWithErrors
runWithLimits act' sess = do
  (set@EvalSettings{..}) <- ask
  let act = do
        liftIO $ nice niceness
        runEvalM act' set
  liftEvalM $ execTimeLimit act set (tmpDirPath </> fileName) sess    

-- | Compiles a source code file using @compileFile@ and writes
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

-- | Result of the deserialization
type DecodeResult = Either String (EvalResult, [EvalError])

-- | Executes an action using time restrictions
execTimeLimit :: Ghc DisplayResult -- ^ Action to be executed
              -> EvalSettings -- ^ Settings with time limit
              -> FilePath -- ^ A path to a temporary file
              -> HscEnv -- ^ Session in which the action will be executed
              -> Ghc (EvalResult, [EvalError])
execTimeLimit act set f sess = do
  pid <- liftIO . forkProcess . flip run' set $ do
    liftIO $ do
      mapM_ setRLimits (rlimits set)
      setupSELinuxCntx (secontext set)      
    setSession sess
    liftEvalM $ runToFile act f
  r <- liftIO $ race (processTimeout pid (timeout set)) $ do
      tc <- getProcessStatus True False pid
      case tc of
        Just exitSt -> do
          r :: DecodeResult <- decode <$> readFile (f ++ ".res")
          case r of
            Right eres -> return eres
            Left str -> return (Left $ "Deserialization error:\n" ++
                                str ++ "\nExit status was: " ++ show exitSt, [])
        Nothing -> do
          signalProcess killProcess pid
          return (Left (show TooLong), [])
  either return return r


