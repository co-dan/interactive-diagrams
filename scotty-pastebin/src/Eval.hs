{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}
module Eval where

import Prelude hiding (writeFile, readFile)

import Control.Monad (when, forever)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, modifyIORef',
                   readIORef, writeIORef)
import Control.Concurrent (threadDelay, ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.ByteString (writeFile, readFile)

import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Types (ProcessID)
import Control.Concurrent.Async (race)
import Data.Serialize (encode, decode)  

import GHC
import GHC.Paths
import DynFlags
import MonadUtils
import Outputable
import Exception
import Panic


import Display
import SignalHandlers
import EvalError


import Debug.Trace
traceM :: Monad m => String -> m ()
traceM s = trace s $ return ()
  

type EvalResult = Either String DisplayResult
newtype EvalQueue =
  EvalQueue (Chan (FilePath, MVar (EvalResult, [EvalError])))


-- | Spawn a new evaluator
prepareEvalQueue :: IO (EvalQueue, ThreadId)
prepareEvalQueue = do
  chan <- EvalQueue <$> newChan
  tid <- forkIO $ do
    run $ do
      compileFile "Preload.hs"
      -- TODO: preloading diagrams
      sess <- getSession
      forever $ handleQueue chan sess
    return ()
  return (chan, tid)
  

sendEvaluator :: EvalQueue -> FilePath -> IO (EvalResult, [EvalError])
sendEvaluator (EvalQueue chan) fpath = do
  mv <- newEmptyMVar
  writeChan chan (fpath, mv)
  takeMVar mv
  
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
  
  
-- | Compiles the 'main' action in the source code file
-- to a @DisplayResult@
compileFile :: FilePath -> Ghc DisplayResult
compileFile file = do
  setTargets =<< sequence [ guessTarget file Nothing
                          , guessTarget "Helper.hs" Nothing]
  graph <- depanal [] False
  -- output graph
  loaded <- load LoadAllTargets
  when (failed loaded) $ throw LoadingException
  setContext (map (IIModule . moduleName . ms_mod) graph)
  let expr = "return . display =<< main"
  ty <- exprType expr -- throws exception if doesn't typecheck
  -- output ty
  res <- unsafePerformIO . unsafeCoerce <$> compileExpr expr  
  return res

-- | Outputs any value that can be pretty-printed using the default style
output :: Outputable a => a -> Ghc ()
output a = do
  dfs <- getSessionDynFlags
  let style = defaultUserStyle
      cntx = initSDocContext dfs style
  liftIO $ print $ runSDoc (ppr a) cntx


-- | Compiles a source code file using @compileFile@ and writes
-- the result to a file
runToFile :: FilePath -> Ghc ()
runToFile f = do
  ref <- liftIO $ newIORef []
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { log_action = logHandler ref }
  dr <- handleException $ compileFile f
  errors <- liftIO $ readIORef ref
  liftIO $ writeFile (f ++ ".res") (encode (dr,errors))
  return ()
  

-- | Waits for a certain period of time (3 seconds)
-- and then kills the process
processTimeout :: ProcessID -- ^ ID of a process to be killed
                  -> IO (EvalResult, [EvalError])
processTimeout pid = do
  threadDelay (3 * 1000000)
  -- putStrLn "Timed out, killing process"
  signalProcess killProcess pid
  return (Left (show TooLong), [])

  
-- | Function that handles requests from the @EvalQueue@
handleQueue :: EvalQueue -> HscEnv -> Ghc ()  
handleQueue (EvalQueue chan) sess = do
  (fpath, resultMVar) <- liftIO $ readChan chan
  traceM $ "Got input: " ++ show fpath
  r <- loadFile fpath sess 
  -- r <- handleException $ compileFile fpath
  traceM $ "Got result: " ++ show r
  liftIO $ putMVar resultMVar r
  return ()

  
-- | Loads the file and compiles it using time restrictions
loadFile :: FilePath -> HscEnv -> Ghc (EvalResult, [EvalError])
loadFile f sess = do
  pid <- liftIO . forkProcess . run' $ do
    setSession sess
    runToFile f
  r <- liftIO $ do
    race (processTimeout pid) $ do
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

