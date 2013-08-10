{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
-- | A non-stripped pooling abstraction that restarts workers
-- Some got has been taken from 'Data.Pool' by bos
module System.Restricted.Worker.Pool
    (
      -- * Workers Pool
      WorkersPool
    , mkPool
      -- * High-level operations on the pool
    , withWorker
      -- * Low-level operations on the pool
    , takeWorker
    , putWorker
    , destroyWorker
    ) where

import Control.Applicative               ((<$>), (<*>))
import Control.Concurrent                (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception                 (mask, onException)
import Control.Monad                     (forM, forever, join, when)
import Control.Monad.Trans.Control       (MonadBaseControl, control)
import System.Mem.Weak                   (addFinalizer)

import System.Restricted.Worker.Internal
import System.Restricted.Worker.Types

-- | A simple pool for workers. Workers are restarted from time to time
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


-- | Create a new workers pool
mkPool :: (Int -> IO (Worker a, RestartWorker IO a))
       -- ^ An action that creates a new worker. Takes a unique number as an argument
       -> Int
       -- ^ Maximum number of workers in the pool
       -> Int
       -- ^ Restart rate (in seconds)
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
    workers' <- forM workers $ \(w, rw) -> (,) <$> rw w <*> return rw
    atomically $ writeTVar wrkrs workers'


-- | Take worker from the pool.
-- The caller is responsible for putting the worker back into the pool
-- or destroying it with 'destroyWorker'
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

-- | Put the worker back in pool
putWorker :: WorkersPool a -> (Worker a, RestartWorker IO a) -> IO ()
putWorker Pool{..} w = atomically $
    modifyTVar' workers (w:)

-- | Destroy a worker. Frees up space in the pool
destroyWorker :: WorkersPool a -> Worker a -> IO ()
destroyWorker Pool{..} w = do
    _ <- killWorker w
    atomically $ modifyTVar' activeWorkers (subtract 1)


-- | Like 'takeWorker' + 'putWorker' but takes care of the exception handling for you
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

