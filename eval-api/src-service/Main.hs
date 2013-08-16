{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TupleSections      #-}
module Main where

import           Control.Applicative                    ((<$>))
import           Control.Concurrent                     (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad                          (forever, unless, void)
import           Data.Default
import           Network                                (Socket, accept)
import           System.Directory                       (createDirectory,
                                                         doesDirectoryExist)
import           System.FilePath.Posix                  ((</>))
import           System.IO                              (BufferMode (..),
                                                         IOMode (..), hClose,
                                                         hSetBuffering,
                                                         openFile, stdin)
import           System.Posix.Files                     (setOwnerAndGroup)
import           System.Posix.Types                     (UserID)
import           System.Posix.User                      (UserEntry (..),
                                                         getUserEntryForName)

import           Debug.Trace
import           Diagrams.Interactive.Eval.EvalSettings
import           Diagrams.Interactive.Eval.EvalWorker
import           SignalHandlers
import           System.Restricted.Worker.Internal
import           System.Restricted.Worker.Pool          (WorkersPool)
import qualified System.Restricted.Worker.Pool          as W
import           System.Restricted.Worker.Protocol
import           System.Restricted.Worker.Types

-- -- | Debug function
-- traceM :: Monad m => String -> m ()
-- traceM s = trace s $ return ()


sockFile :: FilePath
sockFile = "/idia/run/sock/control.sock"

username :: String
username = "vagrant"

workersDir :: FilePath
workersDir = "/idia/run/workers/"

cgroups :: FilePath
cgroups = "/cgroups/cpu/"

limSettings :: LimitSettings
limSettings = def {
     rlimits = Just def {
        totalMemoryLimit = ResourceLimits memlim memlim
        }
     -- , secontext  = Just "idia_restricted_t"
     -- , cgroupPath = Just $ cgroups </> "idiaworkers"
     }
  where memlim = ResourceLimit $ 104857600 * 6
                                 --- 100mb * 6
settings :: EvalSettings
settings = def
  { limitSet     = limSettings
  , pkgDatabases = ["/home/vagrant/.ghc/x86_64-linux-7.7.20130628/package.conf.d"]
  , verbLevel    = 5
  , preloadFile  = "../common/Preload.hs"
  }


setOwner :: FilePath -> UserID -> IO ()
setOwner fp u = setOwnerAndGroup fp u (-1)

newWorkerAct :: Show a => EvalSettings -> a -> IO (Worker EvalWorker, RestartWorker IO EvalWorker)
newWorkerAct wsettings i = do
  let wname = "EvalWorker" ++ show i
      wdir  = workersDir </> ("worker" ++ show i)
  -- uid <- userID <$> getUserEntryForName username
  doesDirectoryExist wdir >>= \e -> unless e $
    createDirectory wdir
  startEvalWorker wname wsettings
  
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  let devnull = openFile "/dev/null" WriteMode
  let set = settings { outHandle = Just devnull }
  putStrLn $ "Using settings = " ++ show set
  pool <- W.mkPool (newWorkerAct set) 1 (60*5)
  currentWorkers <- newMVar []
  soc <- mkSock sockFile
  userID <$> getUserEntryForName username
   >>= setOwner sockFile
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
      (w, rw) <- W.takeWorker pool
      traceM $ "Sending " ++ show w
      _ <- sendData hndl w
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
      W.putWorker pool (wrk',rw)
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
