{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
-- | Library exposing internal functions uses by 'Eval.Worker'
-- useful work writing your own workers
module Eval.Worker.Internal
       (
         -- * Connection related
         connectToWorker, mkSock,
         -- * Specific for certain types of Workers
         sendEvalRequest, performEvalRequest, runToHandle,
         sendEvalRequestNoRestart,
         -- * Useful util functions
         removeFileIfExists
       ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (race)
import Control.Exception (catch, throwIO)
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Serialize (Serialize)
import Network (listenOn, connectTo, PortID(..), Socket)
import System.Directory (doesFileExist, removeFile)
import System.IO (Handle, hClose)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Posix.Files (removeLink)

import GHC

import Eval hiding (runToHandle)
import Eval.EvalM
import Eval.EvalError
import Eval.EvalSettings (EvalSettings(..), LimitSettings(..))
import Eval.Limits
import Eval.Worker.EvalCmd
import Eval.Worker.Protocol
import Eval.Worker.Types

  
connectToWorker :: Worker a -> IO Handle
connectToWorker Worker{..} = connectTo "localhost" (UnixSocket workerSocket)


removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = removeFile f `catch` handleE
  where handleE e
            | isDoesNotExistError e = return ()
            | isPermissionError   e = return ()
            | otherwise             =  putStrLn ("removeFileIfExists " ++ show e)
                                    >> throwIO e                                      

    
mkSock :: FilePath -> IO Socket
mkSock sf = do
  removeFileIfExists sf  
  listenOn (UnixSocket sf)

-----------------------------------------------------------------------  

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
  
-- | Runs a Ghc monad code and outputs the result to a handle  
runToHandle :: (Serialize a, Show a)
            => EvalM a -> Handle -> EvalM (Either String a, [EvalError])
runToHandle act hndl = do
  ref <- liftIO $ newIORef []
  set <- ask
  liftEvalM $ initGhc ref (verbLevel set)
  dr :: Either String a <- handleException act
  errors :: [EvalError] <- liftIO $ readIORef ref
  liftIO $ sendData hndl (dr, errors)
  return (dr, errors)


performEvalRequest :: Handle -> EvalCmd -> IO (DecodeResult EvalResultWithErrors)
performEvalRequest hndl cmd = do
  sendData hndl cmd
  (Right <$> getData hndl) `gcatch` \(e :: ProtocolException) ->
    return (Left (show e))
