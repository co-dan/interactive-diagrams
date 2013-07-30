{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Library exposing internal functions uses by 'Eval.Worker'
-- useful work writing your own workers
module Worker.Internal
    (
      -- * Connection related
      connectToWorker
    , mkSock
      -- * Useful util functions
    , removeFileIfExists
    ) where

import Control.Exception        (catch, throwIO)
import Network                  (PortID (..), Socket, connectTo, listenOn)
import System.Directory         (removeFile)
import System.IO                (Handle)
import System.IO.Error          (isDoesNotExistError, isPermissionError)

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

