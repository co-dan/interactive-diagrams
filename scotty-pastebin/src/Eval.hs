{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}
module Eval where

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

import Display
import SignalHandlers

import Control.Monad

data LoadingException = LoadingException
                      deriving (Typeable)
instance Show LoadingException where
  show LoadingException = "Cannot load the targets"
instance Exception LoadingException

-- LogAction == DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logHandler :: IORef [String] -> LogAction
logHandler ref dflags severity srcSpan style msg =
  case severity of
    SevError ->   modifyIORef' ref (++ [printDoc])
    SevFatal ->   modifyIORef' ref (++ [printDoc])
    _ -> return ()
  where cntx = initSDocContext dflags style
        locMsg = mkLocMessage severity srcSpan msg
        printDoc = show (runSDoc locMsg cntx)
         
handleException :: (ExceptionMonad m, MonadIO m)
                   => m a -> m (Either String a)
handleException m =
  ghandle (\(ex :: SomeException) -> return (Left (showException ex))) $
  handleGhcException (\ge -> return (Left (showGhcException ge ""))) $
  flip gfinally (liftIO restoreHandlers) $
  m >>= return . Right
  
  
run :: Ghc DisplayResult -> IO (Either String DisplayResult)
run m = do
  ref <- newIORef []
  r <- handleException $ run' (initGhc ref >> m)
  logMsg <- unlines <$> readIORef ref
  -- liftIO $ putStrLn log
  case r of
    Left s -> return $ Left $ s ++ "\n" ++ logMsg
    _ -> return r

run' :: Display a => Ghc a -> IO a
run' m = runGhc (Just libdir) m

initGhc :: IORef [String] -> Ghc ()
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
  output graph
  loaded <- load LoadAllTargets
  when (failed loaded) $ throw LoadingException
  setContext (map (IIModule . moduleName . ms_mod) graph)
  let expr = "return . display =<< main"
  ty <- exprType expr -- throws exception if doesn't typecheck
  output ty
  res <- unsafePerformIO . unsafeCoerce <$> compileExpr expr
  return res

output :: Outputable a => a -> Ghc ()
output a = do
  dfs <- getSessionDynFlags
  let style = defaultUserStyle
      cntx = initSDocContext dfs style
  liftIO $ print $ runSDoc (ppr a) cntx
