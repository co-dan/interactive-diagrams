{-# LANGUAGE DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
module Main where

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Data.Typeable (Typeable)
import Data.Default
import Data.ByteString (hGetContents, hPutStr)
import Data.Serialize (encode, decode, Serialize)
import Network (listenOn, connectTo, accept, socketPort, PortID(..), Socket(..))
import Network.Socket (close)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO (Handle, hClose, stdin, stdout, hSetBuffering, BufferMode(..), hGetLine)
import System.Posix.Files (removeLink)

import Eval.Worker
import Eval.EvalSettings


echoHandle :: Handle -> IO ()
echoHandle h = do
  hSetBuffering h LineBuffering
  s <- hGetLine h
  putStrLn $ "Got: " ++ s
  hClose h
  
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  loop =<< startIOWorker "Testworker" "/tmp/1.sock" echoHandle
  return ()

loop :: (Worker IOWorker, RestartWorker IO IOWorker) -> IO ()
loop (w, restart) = do
  putStrLn "Press <ENTER> to restart the worker"
  _ <- getChar
  w' <- restart w
  loop (w', restart)
