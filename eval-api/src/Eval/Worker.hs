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
         startWorker,
         -- ** IOWorker
         IOWorker, startIOWorker,
         -- ** EvalWorker
         EvalWorker, startEvalWorker,
         sendCompileFileRequest, sendEvalStringRequest
       ) where

import Prelude hiding (putStr)
  
import Control.Monad (when, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Data.ByteString (hGetContents, hPutStr, hGetLine, putStr)
import Data.Typeable
import Data.Maybe (isJust)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import Data.Serialize (encode, decode)
import GHC.IO.Handle (hSetBuffering, BufferMode(..), hFlush)
import Network (listenOn, connectTo, accept, PortID(..), Socket)
import Network.Socket (close)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO (Handle, hClose)
import System.Posix.Files (removeLink)
import System.Posix.Process (forkProcess)
import System.Posix.Types (ProcessID)

import DynFlags
import GHC hiding (compileExpr)
import MonadUtils hiding (MonadIO, liftIO)

import Eval
import Eval.Helpers
import Eval.Limits
import Eval.EvalSettings (LimitSettings(..), EvalSettings(..))
import Eval.EvalM
import Eval.Worker.EvalCmd
import Display

-- | A datatype representing a worker of type 'a'
data Worker a = Worker
    { -- | Name of the worker
      workerName     :: String
      -- | A filepath to the Unix socket that will be
      -- used for communicating with the worker.
      -- If the file is already present it will be unliked
      -- during the initializatin step
    , workerSocket   :: FilePath
      -- | Security restrictions for the worker
    , workerLimits   :: LimitSettings
      -- | 'Just pid' if the worker's process ID is 'pid',
      -- Nothing' if the worker is not active/initialized
    , workerPid      :: Maybe ProcessID 
    } deriving (Typeable, Eq, Show)


data IOWorker
data EvalWorker

-- | Check whether the worker is initialized
initialized :: Worker a -> Bool
initialized = isJust . workerPid

-- | Start a general type of worker
startWorker :: Worker a -- ^ A non-active worker
            -> (Socket -> IO ()) 
            -> IO (Worker a)
startWorker w cb = do
  pid <- forkWorker w cb
  return $ w { workerPid = Just pid }

forkWorker :: Worker a -> (Socket -> IO ()) -> IO ProcessID
forkWorker Worker{..} cb = do
  soc <- mkSock workerSocket
  forkProcess $ do
    setLimits workerLimits
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
        hSetBuffering hndl LineBuffering
        act <- evalWorkerAction hndl
        flip run' eset $ do
          setSession sess
          r <- liftEvalM $ runToHandle (runEvalM act eset) hndl
          traceM $ "Got: " ++ show r
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
  (cmd :: DecodeResult EvalCmd) <- decode <$> hGetLine hndl
  case cmd of
    Left str -> error str
    Right x -> return (evalCmdToEvalM x)


-- | Send the 'Worker' a request to compile a file
sendCompileFileRequest :: Worker EvalWorker -> FilePath -> IO EvalResultWithErrors
sendCompileFileRequest w fpath = do
  hndl <- connectToWorker w
  hSetBuffering hndl LineBuffering
  hPutStr hndl (encode (CompileFile fpath))
  hPutStr hndl "\n"
  r :: DecodeResult EvalResultWithErrors <- decode <$> hGetLine hndl
  r `seq` hClose hndl
  case r of
    Right x -> return x
    Left str -> return (Left $ "Deserialization error:\n" ++
                        str, [])

-- | Send the 'Worker' a request to compile an expression
sendEvalStringRequest :: Worker EvalWorker -> String -> IO EvalResultWithErrors
sendEvalStringRequest w str = do
  hndl <- connectToWorker w
  hSetBuffering hndl LineBuffering
  hPutStr hndl (encode (EvalString str))
  hPutStr hndl "\n"
  r :: DecodeResult EvalResultWithErrors <- decode <$> hGetLine hndl
  r `seq` hClose hndl
  case r of
    Right re -> return re
    Left str -> return (Left $ "Deserialization error:\n" ++
                        str, [])
                          
------------------------------------------------------------

connectToWorker :: Worker a -> IO Handle
connectToWorker Worker{..} = connectTo "localhost" (UnixSocket workerSocket)
  
mkSock :: FilePath -> IO Socket
mkSock sf = do
  exists <- doesFileExist sf
  when exists $ removeLink sf
  listenOn (UnixSocket sf)
                  
