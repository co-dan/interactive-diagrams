{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE TypeHoles #-}
-- | A non-stripped pooling abstraction that restarts workers
-- /Note: right now the module is not desgined for threaded applications/
-- Some got has been taken from 'Data.Bool' by bos
module Eval.Worker.RestartingPool where

import Control.Applicative ((<$>))
import Control.Monad (when, join)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)  
import Control.Exception (onException, mask)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Hashable (hash)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Eval.Worker.Types

data WorkersPool a = Pool
    { newWorker     :: Int -> IO (Worker a, RestartWorker IO a)
      -- ^ Action for creating a new worker
      -- ^ Action for restarting a worker
    , maxWorkers    :: Int
      -- ^ Maximum number of initialized workers
    , currentRes    :: TVar Int
    , resources     :: TVar [(Worker a, RestartWorker IO a)]
    }

data WorkerStatus = Idle | InUse

-- data PoolEntry a = PoolEntry
--     { worker        :: MVar (Worker a)
--     , workerStatus  :: MVar WorkerStatus
--     }

mkPool :: (Int -> IO (Worker a, RestartWorker IO a))
       -> Int
       -> IO (WorkersPool a)
mkPool newW maxW = atomically $ do
  res <- newTVar []
  num <- newTVar (0 :: Int)
  return $ Pool newW {-restartW-} maxW num res


takeWorker :: WorkersPool a -> IO (Worker a, RestartWorker IO a)
takeWorker Pool{..} = do
  res <- readTVarIO resources
  case res of
    ((w@Worker{..}, restartW):xs) -> do
      atomically $ writeTVar resources xs
      workerDead <- not <$> workerAlive w
      wrk <- if workerDead
             then do
               restartW w
             else return w
      return (wrk, restartW)
    [] -> join . atomically $ do
      activeRes <- readTVar currentRes
      when (activeRes >= maxWorkers) retry
      modifyTVar' currentRes (+1)
      return $ newWorker (activeRes+1)
        `onException` atomically (modifyTVar' currentRes (subtract 1))



putWorker :: WorkersPool a -> (Worker a, RestartWorker IO a) -> IO ()
putWorker Pool{..} w = atomically $ 
  modifyTVar' resources (w:)
  

destroyWorker :: WorkersPool a -> Worker a -> IO ()
destroyWorker Pool{..} w = do
  _ <- killWorker w
  atomically $ modifyTVar' currentRes (subtract 1)


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
  
