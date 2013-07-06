{-# LANGUAGE DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (when, void, forever)
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

import Eval.Worker
import Eval.EvalSettings
import Eval.Worker.RestartingPool
import Eval.Worker.Protocol
import Eval.Worker.Internal

sockFile :: FilePath
sockFile = "/tmp/control.sock"

echoHandle :: Handle -> IO ()
echoHandle h = do
  hSetBuffering h LineBuffering
  s <- hGetLine h
  putStrLn $ "Got: " ++ s
  hPutStrLn h s
  hClose h

newWorkerAct i = startIOWorker wname wsock echoHandle
  where wname = "Worker" ++ show i
        wsock = "/tmp/" ++ show i ++ ".sock"
        
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  pool <- mkPool newWorkerAct 2
  soc <- mkSock sockFile
  loop soc pool
  return ()

loop :: Socket -> WorkersPool IOWorker -> IO ()
loop soc pool = forever $ do
  (hndl, _, _) <- accept soc
  ln <- hGetLine hndl
  case ln of
    "new" -> void $ forkIO $ do
      (w, rw) <- takeWorker pool
      print w
      return ()
    _     -> do
      hPutStrLn hndl "Dunno"
  hClose hndl
