{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}
--module Eval where
module Main where

import GHC
import GHC.Paths
import DynFlags
import MonadUtils
import Outputable
import Exception
import Panic
import ErrUtils

import Unsafe.Coerce
import System.IO.Unsafe
import Data.Dynamic
import Data.IORef
import System.Posix.Process
import System.Posix.Signals
import Control.Concurrent

import Display
import SignalHandlers
import EvalError

import Control.Monad

-- LogAction == DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logHandler :: IORef [EvalError] -> LogAction
logHandler ref dflags severity srcSpan style msg =
  case srcSpan of
    RealSrcSpan sp -> modifyIORef' ref (++ [err sp])
    UnhelpfulSpan _ -> return ()
  -- case severity of
  --   SevError ->   modifyIORef' ref (++ [printDoc])
  --   SevFatal ->   modifyIORef' ref (++ [printDoc])
  --   _ -> return ()
  where err sp = EvalError severity msg' sp
        cntx = initSDocContext dflags style
        msg' = show (runSDoc msg cntx)
        -- locMsg = mkLocMessage severity srcSpan msg
        -- printDoc = show (runSDoc locMsg cntx)

handleException :: (ExceptionMonad m, MonadIO m)
                   => m a -> m (Either String a)
handleException m =
  ghandle (\(ex :: SomeException) -> return (Left (showException ex))) $
  handleGhcException (\ge -> return (Left (showGhcException ge ""))) $
  flip gfinally (liftIO restoreHandlers) $
  m >>= return . Right
  
  
run :: Ghc a -> IO (Either String a)
run m = do
  ref <- newIORef []
  r <- handleException $ run' (initGhc ref >> m)
  logMsg <- unlines . map show <$> readIORef ref
  case r of
    Left s -> return $ Left $ s ++ "\n" ++ logMsg
    _ -> return r

run' :: Ghc a -> IO a
run' m = runGhc (Just libdir) m

initGhc :: IORef [EvalError] -> Ghc ()
initGhc ref = do
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { hscTarget = HscInterpreted
                           , ghcLink = LinkInMemory
                           , log_action = logHandler ref}
  return ()

compileFile :: FilePath -> Ghc DisplayResult
compileFile file = do
  setTargets =<< sequence [ guessTarget file Nothing
                          , guessTarget "Helper.hs" Nothing]
  graph <- depanal [] False
  -- output graph
  loaded <- load LoadAllTargets
  when (failed loaded) $ throw LoadingException
  setContext (map (IIModule . moduleName . ms_mod) graph)
  let expr = "return . display =<< main"
  ty <- exprType expr -- throws exception if doesn't typecheck
  -- output ty
  res <- unsafePerformIO . unsafeCoerce <$> compileExpr expr
  return res

output :: Outputable a => a -> Ghc ()
output a = do
  dfs <- getSessionDynFlags
  let style = defaultUserStyle
      cntx = initSDocContext dfs style
  liftIO $ print $ runSDoc (ppr a) cntx

loadFile :: FilePath -> IO (Either String String)
loadFile f = run $ do
  sess <- getSession
  pid <- liftIO . forkProcess . run' $ do
    setSession sess
    dr <- compileFile f
    liftIO $ writeFile (f ++ ".res") (show dr)
    return ()
  liftIO $ do
    threadDelay (2 * 1000000)
    tc <- getProcessStatus False False pid
    print tc
    case tc of
      Just _ -> do
        r <- readFile (f ++ ".res")
        return r
      Nothing -> do
        signalProcess killProcess pid
        throw TooLong

main = print =<< loadFile "./test/file1.hs"
