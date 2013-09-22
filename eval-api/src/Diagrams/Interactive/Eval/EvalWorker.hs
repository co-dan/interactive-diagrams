{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
module Diagrams.Interactive.Eval.EvalWorker
    (
      -- * Eval worker
      EvalWorker
    , startEvalWorker
      -- ** Communication
    , sendEvalRequest
    , sendEvalRequestNoRestart
    , sendCompileFileRequest
    , sendEvalStringRequest
      -- * Eval Cmd
    , EvalCmd(..)
    , evalCmdToEvalM
      -- * Other stuff
    , EvalResultWithErrors
    , WStatus(..)
    , ServiceCmd(..)
    ) where

import           Control.Applicative                    ((<$>), (<*>))
import           Control.Concurrent.Async               (race)
import           Control.Exception                      (catch, throwIO)
import           Control.Monad                          (forever)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Reader                   (ask)
import           Data.IORef                             (newIORef, readIORef)
import           Data.Maybe                             (fromJust)
import           Data.Serialize                         (Serialize)
import           Data.Text                              (Text)
import qualified Data.Text.IO                           as T
import           Data.Typeable
import           GHC.Generics
import           Network                                (accept)
import           System.Directory                       (createDirectory, removeDirectoryRecursive)
import           System.FilePath.Posix                  ((</>))
import           System.IO                              (Handle, hClose)
import           System.IO.Error                        (isAlreadyExistsError,
                                                         isPermissionError)

import           Diagrams.Interactive.Display
import           Diagrams.Interactive.Eval.EvalError
import           Diagrams.Interactive.Eval.EvalM
import           Diagrams.Interactive.Eval.EvalSettings
import           Diagrams.Interactive.Eval.Handlers
import           Diagrams.Interactive.Eval.Helpers
import           GHC                                    hiding (compileExpr)
import           SignalHandlers
import           System.Restricted.Limits
import           System.Restricted.Worker
import           System.Restricted.Worker.Internal

-- | Evaluation result together with a list of errors/warnings
type EvalResultWithErrors = (EvalResult, [EvalError])

{- | A type of worker that evaluates code.
Stores the GHC session for preloading

@
instance WorkerData EvalWorker where
    type WData EvalWorker = 'HscEnv'
    type WMonad EvalWorker = IO
@

-}
data EvalWorker

instance WorkerData EvalWorker where
    type WData EvalWorker = HscEnv
    type WMonad EvalWorker = IO

-- | Starts a specialized worker for running EvalM
-- preloads stuff, etc
startEvalWorker :: String                   -- ^ Name of the worker
                -> EvalSettings             -- ^ Evaluation settings that will be used
                -> IO (Worker EvalWorker, RestartWorker IO EvalWorker)
startEvalWorker name eset = startWorker name sock out set pre callback
  where sock = tmpDirPath eset </> (name ++ ".sock")
        set  = limitSet eset
        -- uid  = processUid set
        out  = outHandle eset
        cleanup = do
            let dir = (++) <$> chrootPath set <*> return (tmpDirPath eset)
            case dir of
                Nothing -> return ()
                Just path -> do
                    removeDirectoryRecursive path `catch` ignore
                    createDirectory path `catch` ignore
        ignore e = if isPermissionError e || isAlreadyExistsError e
                   then return ()
                   else throwIO e
        pre  = flip run' eset $ do
            liftIO cleanup
            addPkgDbs (pkgDatabases eset)
            dfs <- getSessionDynFlags
            _ <- setSessionDynFlags $ dfs { hscTarget = HscInterpreted
                                          , ghcLink   = LinkInMemory
                                          , verbosity = verbLevel eset
                                          }
            oldTrgs <- getTargets
            loadFile (preloadFile eset)
            setTargets oldTrgs
            liftIO restoreHandlers
            getSession
        callback sess soc = forever $ do
            (hndl, _, _) <- accept soc
            act <- evalWorkerAction hndl
            flip run' eset $ do
              setSession sess
              r :: EvalResultWithErrors <- runToHandle act hndl
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

-- | Runs a Ghc monad code and outputs the result to a handle
runToHandle :: (Serialize a)
            => EvalM a -> Handle -> EvalM (Either String a, [EvalError])
runToHandle act hndl = do
    ref <- liftIO $ newIORef []
    set <- ask
    liftGhc $ initGhc ref (verbLevel set)
    dflags <- getSessionDynFlags
    dr :: Either String a <- defaultCleanupHandler dflags (handleException act)
    errors :: [EvalError] <- liftIO $ readIORef ref
    _ <- liftIO $ sendData hndl (dr, errors)
    return (dr, errors)


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
        False -> restart w
    let evres = case r of
            Left _ -> (Left "Process timedout", [])
            Right (Right x) ->  x
            Right (Left str) -> (Left $ "Deserialization error:\n" ++
                                 str, [])
    return (evres, w')

-- | Just like 'sendEvalRequest' but does not restart the worker if the limits were hit
sendEvalRequestNoRestart :: Worker EvalWorker
                         -> EvalCmd
                         -> IO (EvalResultWithErrors, WStatus)
sendEvalRequestNoRestart w cmd = do
    hndl <- connectToWorker w
    let pid = fromJust . workerPid $ w
    let timelimit = timeout . workerLimits $ w
    -- r <- performEvalRequest hndl cmd
    r <- race (processTimeout pid timelimit) $ do
        requestResult <- performEvalRequest hndl cmd
        return requestResult
    r `seq` hClose hndl
    let evres = case r of
            Left _ -> ((Left "Process timedout", []), Timeout)
            Right (Right x) ->  (x, OK)
            Right (Left str) -> ((Left $ "Deserialization error:\n" ++
                                  str, []), Unknown)
    return evres



performEvalRequest :: Handle -> EvalCmd -> IO (DecodeResult EvalResultWithErrors)
performEvalRequest hndl cmd = do
    _ <- sendData hndl cmd
    (Right <$> getData hndl) `gcatch` \(e :: ProtocolException) ->
        return (Left (show e))


------------------------------------------------------------

-- | Datatype used for communicating with the 'EvalWorker'
data EvalCmd = CompileFile FilePath
               -- ^ Compile a Haskell module. Takes the path to the file
             | EvalString  String
               -- ^ Evaluate a string. Takes the expression to evaluate
             | EvalFile    String    Text
               -- ^  Similar to 'CompileFile'. Takes the name of the file, contents
             deriving (Typeable, Generic)

instance Serialize EvalCmd

-- | Convert an 'EvalCmd' to 'EvalM' action that can be executed
evalCmdToEvalM :: EvalCmd -> EvalM DisplayResult
evalCmdToEvalM (CompileFile fpath) = do
  loadFile fpath
  interactive <- needsInput "example"
  ty <- exprType "example"
  underIO <- isUnderIO ty
  if interactive
      then fmap Interactive $ do
           sess <- getSession
           compileToJS fpath
               `gfinally` setSession sess
      else fmap Static $ if underIO
           then compileExpr "(return . display =<< example) :: IO StaticResult"
           else compileExpr "(display example) :: StaticResult"
                
evalCmdToEvalM (EvalString s) = fmap Static $ compileExpr s
evalCmdToEvalM (EvalFile n txt) = do
  EvalSettings{..} <- ask
  let fpath = tmpDirPath </> n
--  traceM fpath
  liftIO $ T.writeFile fpath txt
  evalCmdToEvalM (CompileFile fpath)

-- | Worker status
data WStatus = OK | Timeout | Unknown
             deriving (Generic, Show)

instance Serialize WStatus

-- | Commands for the service app
data ServiceCmd = RequestWorker
                | RequestWorkerMaybe
                | ReturnWorker WStatus (Worker EvalWorker)
                deriving (Generic)

instance Serialize ServiceCmd
