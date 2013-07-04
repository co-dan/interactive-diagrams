{-# LANGUAGE DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies, StandaloneDeriving, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, EmptyDataDecls, ScopedTypeVariables #-}
{-# LANGUAGE TypeHoles #-}
module Eval.Worker where

import Prelude hiding (putStr)
  
import Control.Monad (when, forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Data.ByteString (hGetContents, hPutStr, hGetLine, putStr)
import Data.Typeable
import Data.Maybe (isJust)
import Data.Serialize (encode, decode, Serialize)
import Network (listenOn, connectTo, accept, socketPort, PortID(..), Socket(..))
import Network.Socket (close)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO (Handle, hClose)
import System.Posix.Files (removeLink)
import System.Posix.Process (nice, forkProcess, getProcessStatus)
import System.Posix.Types (ProcessID)

import DynFlags
import Exception
import GHC hiding (compileExpr)
import MonadUtils hiding (MonadIO, liftIO)
import Outputable
import Panic



import Eval
import Eval.Helpers
import Eval.Limits
import Eval.EvalSettings (LimitSettings(..), EvalSettings(..))
import Eval.EvalM
import Eval.Worker.EvalCmd
import Display

data Worker a = Worker
    { workerName     :: String
    , workerSocket   :: FilePath
    , workerLimits   :: LimitSettings
    -- | 'Nothing' if the worker is not active
    , workerPid      :: Maybe ProcessID 
    } deriving (Typeable, Eq, Show)


data IOWorker
data EvalWorker

isActive :: Worker a -> Bool
isActive = isJust . workerPid

-- | Starts a general type of worker
startWorker :: Worker a -- ^ A non-active worker
            -> (Socket -> IO ()) 
            -> IO (Worker a)
startWorker w cb = do
  pid <- forkWorker w cb
  return $ w { workerPid = Just pid }

forkWorker :: Worker a -> (Socket -> IO ()) -> IO ProcessID
forkWorker Worker{..} cb = do
  soc <- mkSock workerSocket
  soc `seq` forkProcess $ do
    setLimits workerLimits
    cb soc
    return ()


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
        act <- evalWorkerAction hndl
        r <- flip run' eset $ do
          setSession sess
          dr <- handleException act
          liftIO $ print dr
          return dr
        -- hClose hndl
        return r
  return $ worker { workerPid = Just pid }
  where worker = Worker { workerName   = name
                        , workerSocket = tmpDirPath eset </> (name ++ ".sock")
                        , workerLimits = limitSet eset
                        , workerPid    = Nothing
                        }


evalWorkerAction :: Handle -> IO (EvalM DisplayResult)
evalWorkerAction hndl = do
  (cmd :: DecodeResult EvalCmd) <- decode <$> hGetContents hndl
  case cmd of
    Left str -> error str
    Right x -> return (evalCmdToEvalM x)

    
sendCompileFileRequest :: Worker EvalWorker -> FilePath -> IO DisplayResult
sendCompileFileRequest w fpath = do
  hndl <- connectToWorker w
  hPutStr hndl (encode (CompileFile fpath))
  return (DisplayResult [])
                          

sendEvalStringRequest :: Worker EvalWorker -> String -> IO DisplayResult
sendEvalStringRequest w str = do
  hndl <- connectToWorker w
  hPutStr hndl (encode (EvalString str))
  putStr =<< hGetLine hndl
  return (DisplayResult [])
                          
------------------------------------------------------------

connectToWorker :: Worker a -> IO Handle
connectToWorker Worker{..} = connectTo "localhost" (UnixSocket workerSocket)
  
mkSock :: FilePath -> IO Socket
mkSock sf = do
  exists <- doesFileExist sf
  when exists $ removeLink sf
  listenOn (UnixSocket sf)
                  
