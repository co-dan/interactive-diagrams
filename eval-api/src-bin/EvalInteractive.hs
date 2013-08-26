{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.List
import qualified Data.Text.Lazy                         as TL
import           Data.Time.Clock                        (diffUTCTime,
                                                         getCurrentTime)
import           System.Console.Readline

import           Diagrams.Interactive.Display
import           Diagrams.Interactive.Eval
import           Diagrams.Interactive.Eval.EvalError
import           Diagrams.Interactive.Eval.EvalM
import           Diagrams.Interactive.Eval.EvalSettings
import           Diagrams.Interactive.Eval.EvalWorker
import           Diagrams.Interactive.Eval.Helpers
    

settings :: EvalSettings
settings = def {
  limitSet = def {
     rlimits = Just def {
        totalMemoryLimit = ResourceLimits memlim memlim
        },
     secontext = Nothing,
     timeout = 20
     }
  }
  where memlim = ResourceLimit $ 104857600 * 10
                                 --- 100mb * 10


main :: IO ()
main = startEvalWorker "evali" settings
       >>= loop "> "

loop :: String -> (Worker EvalWorker, RestartWorker IO EvalWorker) -> IO ()
loop c (worker, restart) = do
  mbln <- readline c
  case mbln of
    Nothing -> return ()
    Just ":q" -> cleanUp worker >> return ()
    Just ":quit" -> cleanUp worker >> return ()
    Just ln -> do
      addHistory ln
      worker' <- measureTime $ do
        ((r, errors), w') <- evalLn ln (worker, restart)
        case r of
          Right (Static (StaticResult res)) ->
            mapM_ (putStr . TL.unpack . result) res
            >> putStrLn ""
          Right (Interactive _) -> putStrLn "interactive result"
          Left err -> putStrLn err
        putStrLn "Errors:"
        mapM_ print errors
        return w'
      loop c (worker', restart)

evalLn :: String
       -> (Worker EvalWorker, RestartWorker IO EvalWorker)
       -> IO (EvalResultWithErrors, Worker EvalWorker)
evalLn s wrk
  | ":load" `isPrefixOf` s =
    let fname = dropWhile (==' ') $ drop 5 s
    in sendCompileFileRequest wrk fname
  | otherwise = let expr = ("(return $ display (" ++ s ++ ")) :: IO StaticResult")
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
