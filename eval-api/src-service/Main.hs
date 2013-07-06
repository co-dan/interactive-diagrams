{-# LANGUAGE DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar  
import Control.Monad (when, void, forever, liftM)
import Data.ByteString (hGetContents)
import Data.Default
import Data.Serialize (encode, decode, Serialize)
import Data.Typeable (Typeable)
import Network (listenOn, connectTo, accept, socketPort, PortID(..), Socket(..))
import Network.Socket (close)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO (Handle, hClose, stdin, stdout, hSetBuffering, BufferMode(..), hGetLine, hPutStrLn)
import System.Posix.Files (removeLink)

import Eval (traceM)
import Eval.Worker
import Eval.EvalSettings
import Eval.Worker.Types
import Eval.Worker.RestartingPool
import Eval.Worker.Protocol
import Eval.Worker.Internal
import Eval.Worker.EvalCmd
import SignalHandlers

sockFile :: FilePath
sockFile = "/tmp/control.sock"

settings :: EvalSettings
settings = def {
  limitSet = def {
     rlimits = Just def {
        totalMemoryLimit = ResourceLimits memlim memlim
        },
     secontext = Nothing
     }
  }
  where memlim = ResourceLimit $ 104857600 * 2
                                 --- 100mb * 2

newWorkerAct i = startEvalWorker wname settings
  where wname = "EvalWorker" ++ show i

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  pool <- mkPool newWorkerAct 2
  currentWorkers <- newMVar []
  soc <- mkSock sockFile
  loop soc pool currentWorkers
  return ()

loop :: Socket
     -> WorkersPool EvalWorker
     -> MVar [(Worker EvalWorker, RestartWorker IO EvalWorker)]
     -> IO ()
loop soc pool currentWorkers = forever $ do
  restoreHandlers 
  (hndl, _, _) <- accept soc
  cmd <- getData hndl
  case cmd of
    RequestWorker -> void $ forkIO $ do
      (w, rw) <- takeWorker pool
      traceM $ "Sending " ++ show w
      sendData hndl w
      modifyMVar_ currentWorkers (return . ((w,rw):))
      hClose hndl
    ReturnWorker st wrk -> do
      traceM $ "Returning " ++ show wrk
      hClose hndl
      workers <- takeMVar currentWorkers
      (wrk',rw) <- case spanMaybe ((==wrk) . fst) workers of
        (Nothing, _) -> error "Unknown worker"
        (Just (_, rw), workers') -> do
          putMVar currentWorkers workers'
          traceM $ "Status " ++ show st
          case st of
            OK -> return (wrk, rw)
            Timeout -> restartIfDead rw wrk
            Unknown -> restartIfDead rw wrk
      putWorker pool (wrk',rw)
    _     -> error "Unimplemented"

restartIfDead :: RestartWorker IO a -> Worker a
              -> IO (Worker a, RestartWorker IO a)
restartIfDead rw wrk = do
  dead <- not <$> workerAlive wrk
  case dead of
    True -> rw wrk >>= return . (, rw)
    False -> return (wrk, rw)

spanMaybe :: (a -> Bool) -> [a] -> (Maybe a, [a])
spanMaybe f lst = case span f lst of
  (x:xs, ys) -> (Just x, ys ++ xs)
  ([],   ys) -> (Nothing, ys)
