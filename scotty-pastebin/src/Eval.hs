{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Eval where

import Prelude hiding (writeFile, readFile)

import Control.Monad (when, forever)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState(..))
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, modifyIORef',
                   readIORef)
import Control.Concurrent (threadDelay, ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.ByteString (writeFile, readFile)
import System.FilePath.Posix ((</>))
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Types (ProcessID)
import Control.Concurrent.Async (race)
import Data.Serialize (encode, decode, Serialize)  
import Data.Default
  
import GHC
import GHC.Paths
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

-- | Runs a Ghc monad and returns either a result, or an error message
run :: Ghc a -> IO (Either String a)
run m = do
  ref <- newIORef []
  r <- handleException $ run' (initGhc ref >> m)
  logMsg <- unlines . map show <$> readIORef ref
  case r of
    Left s -> return $ Left $ s ++ "\n" ++ logMsg
    _ -> return r

run' :: Ghc a -> IO a
run' m = runGhc (Just libdir) m

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
  m >>= return . Right
  


-- | Spawn a new evaluator
prepareEvalQueue :: IO (EvalQueue, ThreadId)
prepareEvalQueue = do
  chan <- EvalQueue <$> newChan
  tid <- forkIO $ do
    run $ do
      evalEvalM $ loadFile "Preload.hs"
      sess <- getSession
      forever $ handleQueue chan sess
    return ()
  return (chan, tid)
  

sendEvaluator :: EvalQueue -> EvalM DisplayResult -> IO (EvalResult, [EvalError])
sendEvaluator (EvalQueue chan) act = do
  mv <- newEmptyMVar
  writeChan chan (act, mv)
  takeMVar mv
  
  
-- | Function that handles requests from the @EvalQueue@
handleQueue :: EvalQueue -> HscEnv -> Ghc ()  
handleQueue (EvalQueue chan) sess = do
  (act', resultMVar) <- liftIO $ readChan chan
  -- r <- loadFile fpath sess
  r <- runWithLimits act' sess
  liftIO $ putMVar resultMVar r
  return ()


runWithLimits :: EvalM DisplayResult -> HscEnv -> Ghc EvalResultWithErrors
runWithLimits act' sess = evalEvalM $ do
  let act = evalEvalM act'
  EvalSettings{..} <- get
  liftEvalM $ execTimeLimit act timeout (tmpDirPath </> fileName) sess    

-- | Compiles a source code file using @compileFile@ and writes
-- the result to a file
runToFile :: Serialize a => Ghc a -> FilePath -> Ghc ()
runToFile act f = do
  ref <- liftIO $ newIORef []
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { log_action = logHandler ref }
  dr <- handleException $ act
  errors <- liftIO $ readIORef ref
  liftIO $ writeFile (f ++ ".res") (encode (dr,errors))
  return ()

-- | Executes an action using time restrictions
execTimeLimit :: Ghc DisplayResult -- ^ Action to be executed
              -> Int -- ^ Time limit
              -> FilePath -- ^ A path to a temporary file
              -> HscEnv -- ^ Session in which the action will be executed
              -> Ghc (EvalResult, [EvalError])
execTimeLimit act lim f sess = do
  pid <- liftIO . forkProcess . run' $ do
    setSession sess
    runToFile act f
  r <- liftIO $ do
    race (processTimeout pid lim) $ do
      tc <- getProcessStatus True False pid
      -- print tc
      case tc of
        Just _ -> do
          Right (r :: (EvalResult, [EvalError])) <- 
            decode <$> readFile (f ++ ".res")
          return r
        Nothing -> do
          signalProcess killProcess pid
          return (Left (show TooLong), [])
  either return return r


-- | Outputs any value that can be pretty-printed using the default style
output :: Outputable a => a -> Ghc ()
output a = do
  dfs <- getSessionDynFlags
  let style = defaultUserStyle
      cntx = initSDocContext dfs style
  liftIO $ print $ runSDoc (ppr a) cntx
