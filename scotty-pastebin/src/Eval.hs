{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Eval where

import GHC
import GHC.Paths
import HscTypes
import Data.Dynamic
import MonadUtils
import Outputable
import Exception
import Panic
import Unsafe.Coerce
import System.IO.Unsafe

import Display
import SignalHandlers

import Control.Applicative
import Control.Concurrent
import System.Posix.Process  

import Control.Error.Util hiding (tryIO)


handleException :: (ExceptionMonad m, MonadIO m)
                   => m a -> m (Either String a)
handleException m =
  ghandle (\(ex :: SomeException) -> return (Left (show ex))) $
  handleGhcException (\ge -> return (Left (show ge))) $
  flip gfinally (liftIO restoreHandlers) $
  m >>= return . Right
  
  
-- run :: Display a => Ghc a -> IO a
run :: Ghc DisplayResult -> IO (Either String DisplayResult)
run m = handleException $ run' (initGhc >> m)

-- run' :: Display a => Ghc a -> IO a
run' m = runGhc (Just libdir) m

output :: Outputable a => a -> Ghc ()
output a = do
  dfs <- getSessionDynFlags
  let style = defaultUserStyle
      cntx = initSDocContext dfs style
  liftIO $ print $ runSDoc (ppr a) cntx

initGhc :: Ghc ()
initGhc = do
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { hscTarget = HscInterpreted
                           , ghcLink = LinkInMemory }
  return ()

compileFile :: FilePath -> Ghc DisplayResult
compileFile file = do
  setTargets =<< sequence [ guessTarget file Nothing
                          , guessTarget "Helper.hs" Nothing]
  -- output target
  graph <- depanal [] False
  loaded <- load LoadAllTargets
  -- when (failed loaded)
  setContext (map (IIModule . moduleName . ms_mod) graph)
  let expr = "return . display =<< main"
  ty <- exprType expr -- throws exception if doesn't typecheck
  -- output ty
  res <- unsafePerformIO . unsafeCoerce <$> compileExpr expr
  return res

test :: IO DisplayResult
test = do
  d <- run (compileFile "./test/file1.hs")
  case d of
    Left _ -> error "Err"
    Right r -> return r
