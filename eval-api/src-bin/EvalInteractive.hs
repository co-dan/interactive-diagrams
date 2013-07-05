{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import qualified Data.Text.Lazy as TL
import Control.Concurrent
import Data.Default
import System.Console.Readline
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Eval
import Display
import Eval.EvalM
import Eval.EvalSettings
import Eval.Helpers  
import Eval.EvalError
import Eval.Worker

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


main :: IO ()
main = startEvalWorker "evali" settings
       >>= loop "> "

loop :: String -> Worker EvalWorker -> IO ()
loop c worker = do
  mbln <- readline c
  case mbln of
    Nothing -> return ()
    Just ":q" -> cleanUp worker >> return ()
    Just ":quit" -> cleanUp worker >> return ()
    Just ln -> do
      addHistory ln
      worker' <- measureTime $ do
        ((r, errors), w') <- evalLn ln worker
        case r of
          Right (DisplayResult res) ->
            mapM_ (putStr . TL.unpack . result) res
            >> putStrLn ""
          Left err -> putStrLn err
        putStrLn "Errors:"
        mapM_ print errors
        return w'
      loop c worker'

evalLn :: String -> Worker EvalWorker -> IO (EvalResultWithErrors, Worker EvalWorker)
evalLn s wrk
  | ":load" `isPrefixOf` s = 
    let fname = dropWhile (==' ') $ drop 5 s
    in sendCompileFileRequest wrk fname
  | otherwise = let expr = ("(return $ display (" ++ s ++ ")) :: IO DisplayResult")
                in sendEvalStringRequest wrk expr

cleanUp :: Worker a -> IO ()
cleanUp w = void $ killWorker w 
                   
measureTime :: MonadIO m => m a -> m a
measureTime act = do
  t0 <- liftIO getCurrentTime
  res <- act
  t1 <- liftIO getCurrentTime
  liftIO $ putStrLn $ "Time elapsed: " ++ show (diffUTCTime t1 t0)
  return res
