module Eval.Limits where

import Control.Concurrent (threadDelay)
import System.Posix.Signals (signalProcess, killProcess)
import System.Posix.Types (ProcessID)

import Eval.EvalM
import Eval.EvalError


-- | Waits for a certain period of time (3 seconds)
-- and then kills the process
processTimeout :: ProcessID -- ^ ID of a process to be killed
               -> Int -- ^ Time limit (in seconds)
               -> IO (EvalResult, [EvalError])
processTimeout pid lim = do
  threadDelay (lim * 1000000)
  -- putStrLn "Timed out, killing process"
  signalProcess killProcess pid
  return (Left (show TooLong), [])

