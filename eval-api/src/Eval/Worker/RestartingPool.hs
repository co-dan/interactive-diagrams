{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
-- | A non-stripped pooling abstraction that restarts workers
-- Some got has been taken from 'Data.Pool' by bos
module Eval.Worker.RestartingPool where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Control.Exception (onException, mask)
import Control.Monad (when, join, forever, forM)
import Control.Monad.Trans.Control (MonadBaseControl, control)  
import System.Mem.Weak (addFinalizer)

import Eval (traceM)
import Eval.Worker.Types

data WorkersPool a = Pool
    { newWorker     :: Int -> IO (Worker a, RestartWorker IO a)
      -- ^ Action for creating a new worker
    , maxWorkers    :: Int
      -- ^ Maximum number of initialized workers
    , activeWorkers :: TVar Int
      -- ^ Current number of active workers
    , workers       :: TVar [(Worker a, RestartWorker IO a)]
      -- ^ A list of Workers
    , restartRate   :: Int
      -- ^ How long we should wait before restarting the workers (in seconds)
    }

data WorkerStatus = Idle | InUse

mkPool :: (Int -> IO (Worker a, RestartWorker IO a))
       -> Int
       -> Int
       -> IO (WorkersPool a)
mkPool newW maxW restartRate = do
  res <- atomically $ newTVar []
  num <- atomically $ newTVar (0 :: Int)
  reaperT <- forkIO $ reaper res restartRate
  let p = Pool newW maxW num res restartRate
  addFinalizer p (killThread reaperT)
  return p


reaper :: TVar [(Worker a, RestartWorker IO a)] -> Int -> IO ()
reaper wrkrs t' = forever $ do
  let t = t' * 1000000
  threadDelay t
  workers <- readTVarIO wrkrs
  print $ length workers
  workers' <- forM workers $ \(w, rw) -> (,) <$> rw w <*> return rw
  atomically $ writeTVar wrkrs workers'
    
  
takeWorker :: WorkersPool a -> IO (Worker a, RestartWorker IO a)
takeWorker Pool{..} = do
  res <- readTVarIO workers
  case res of
    ((w@Worker{..}, restartW):xs) -> do
      atomically $ writeTVar workers xs
      workerDead <- not <$> workerAlive w
      wrk <- if workerDead
             then do
               restartW w
             else return w
      return (wrk, restartW)
    [] -> join . atomically $ do
      activeRes <- readTVar activeWorkers
      when (activeRes >= maxWorkers) retry
      modifyTVar' activeWorkers (+1)
      return $ newWorker (activeRes+1)
        `onException` atomically (modifyTVar' activeWorkers (subtract 1))



putWorker :: WorkersPool a -> (Worker a, RestartWorker IO a) -> IO ()
putWorker Pool{..} w = atomically $ 
  modifyTVar' workers (w:)
  

destroyWorker :: WorkersPool a -> Worker a -> IO ()
destroyWorker Pool{..} w = do
  _ <- killWorker w
  atomically $ modifyTVar' activeWorkers (subtract 1)


withWorker :: (MonadBaseControl IO m)
           => WorkersPool a
           -> ((Worker a, RestartWorker IO a) -> m b)
           -> m b
withWorker pool cb = control $ \runIO -> mask $ \restore -> do
  res <- takeWorker pool
  -- ret <- cb res
  ret <- restore (runIO (cb res))
           `onException` destroyWorker pool (fst res)
  putWorker pool res
  return ret
  
