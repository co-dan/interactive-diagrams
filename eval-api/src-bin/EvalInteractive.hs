{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Default
import System.Console.Readline
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Eval
import Display
import Eval.EvalM
import Eval.EvalSettings
import Eval.Helpers  
import Eval.EvalError

main :: IO ()
main = liftM fst (prepareEvalQueue def)
       >>= loop "> "

loop :: String -> EvalQueue -> IO ()
loop c q = do
  mbln <- readline c
  case mbln of
    Nothing -> return ()
    Just ":q" -> return ()
    Just ":quit" -> return ()
    Just ln -> do
      addHistory ln
      measureTime $ 
        print =<< sendEvaluator q (evalLn ln)
      loop c q

evalLn :: String -> EvalM DisplayResult
evalLn s
  | ":load" `isPrefixOf` s = 
    let fname = dropWhile (==' ') $ drop 5 s
    in (loadFile fname >> compileExpr "return . display =<< main")
  | otherwise = compileExpr ("(return $ display (" ++ s ++ ")) :: IO DisplayResult")


measureTime :: MonadIO m => m a -> m a
measureTime act = do
  t0 <- liftIO getCurrentTime
  res <- act
  t1 <- liftIO getCurrentTime
  liftIO $ putStrLn $ "Time elapsed: " ++ show (diffUTCTime t1 t0)
  return res
