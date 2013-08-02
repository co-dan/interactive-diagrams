{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Library exposing internal functions uses by 'Eval.Worker'
-- useful work writing your own workers
module Worker.Internal
    (
      forkWorker
      -- * Connection related
    , connectToWorker
    , mkSock
      -- * Useful util functions
    , removeFileIfExists
    ) where

import Control.Exception      (catch, throwIO)
import Control.Monad          (void)
import Network                (PortID (..), Socket, connectTo, listenOn)
import Network.Socket         (close)
import System.Directory       (removeFile)
import System.IO              (Handle)
import System.IO.Error        (isDoesNotExistError, isPermissionError)
import System.Mem.Weak        (addFinalizer)
import System.Posix.IO        (dupTo, handleToFd)
import System.Posix.Process   (forkProcess)
import System.Posix.Signals   (Handler (..), installHandler,
                               processStatusChanged, setStoppedChildFlag)
import System.Posix.Types     (Fd (..), ProcessID)

import Worker.Internal.Limits
import Worker.Types

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

forkWorker :: Worker a -> Maybe (IO Handle) -> (Socket -> IO ()) -> IO ProcessID
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


