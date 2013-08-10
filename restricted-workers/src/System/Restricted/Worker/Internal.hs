{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Library exposing internal functions uses by 'Eval.Worker'
-- useful work writing your own workers
module System.Restricted.Worker.Internal
    (
      -- * Worker related
      forkWorker
    , killWorker
    , workerAlive
      -- * Connection related
    , connectToWorker
    , mkSock
      -- * Useful util functions
    , removeFileIfExists
    , processAlive
    ) where

import Control.Exception              (IOException, catch, handle, throwIO)
import Control.Monad                  (void, when)
import Data.Maybe                     (fromJust)
import Network                        (PortID (..), Socket, connectTo, listenOn)
import Network.Socket                 (close)
import System.Directory               (removeFile)
import System.IO                      (Handle)
import System.IO.Error                (isDoesNotExistError, isPermissionError)
import System.Mem.Weak                (addFinalizer)
import System.Posix.IO                (dupTo, handleToFd)
import System.Posix.Process           (forkProcess, getProcessStatus)
import System.Posix.Signals           (Handler (..), installHandler,
                                       killProcess, processStatusChanged,
                                       setStoppedChildFlag, signalProcess)
import System.Posix.Types             (Fd (..), ProcessID)

import System.Restricted.Limits
import System.Restricted.Worker.Types

-- | Connect to the worker's socket and return a handle
connectToWorker :: Worker a -> IO Handle
connectToWorker Worker{..} = connectTo "localhost" (UnixSocket workerSocket)

-- | Remove a file if it exists. Should be thread-safe.
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = removeFile f `catch` handleE
  where handleE e
            | isDoesNotExistError e = return ()
            | isPermissionError   e = return ()
            | otherwise             =  putStrLn ("removeFileIfExists " ++ show e)
                                    >> throwIO e

-- | Create a new unix socket
mkSock :: FilePath -> IO Socket
mkSock sf = do
    removeFileIfExists sf
    listenOn (UnixSocket sf)

-- | Fork a worker process
forkWorker :: Worker a
           -> Maybe (IO Handle)  -- ^ Where to redirect stdout
           -> (Socket -> IO ())  -- ^ Callback funcion
           -> IO ProcessID
forkWorker (w@Worker{..}) out cb = do
    _ <- setStoppedChildFlag True
    _ <- installHandler processStatusChanged Ignore Nothing
    soc <- mkSock workerSocket
    addFinalizer w (close soc)
    forkProcess $ do
        _ <- setStoppedChildFlag False
        _ <- installHandler processStatusChanged Default Nothing
        setLimits workerLimits
        case out of
            Nothing -> return ()
            Just x  -> do
                fd <- handleToFd =<< x
                void $ dupTo fd (Fd 1)
        cb soc



-- | Kill a worker. Takes an initialized worker,
-- returns non-initialized one.
killWorker :: Worker a -> IO (Worker a)
killWorker w@Worker{..} = do
    when (initialized w) $ do
        alive <- processAlive (fromJust workerPid)
        when alive $ do
            signalProcess killProcess (fromJust workerPid)
            tc <- getProcessStatus False False (fromJust workerPid)
            case tc of
                Just _  -> return ()
                Nothing -> signalProcess killProcess (fromJust workerPid)
    return (w { workerPid = Nothing })


-----------------------

-- | Checks whether the process is alive
-- /hacky/
processAlive :: ProcessID -> IO Bool
processAlive pid = handle (\(_ :: IOException) -> return False) $ do
    _ <- getProcessStatus False False pid
    return True

-- | Checks whether the worker is alive
workerAlive :: Worker a -> IO Bool
workerAlive w = do
    case (workerPid w) of
        Nothing  -> return False
        Just pid -> processAlive pid

