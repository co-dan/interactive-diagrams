{-# LANGUAGE CPP #-}
{-|
  Helper functions for the 'EvalM' and 'Ghc' monads
-}
module Eval.Helpers where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)

import GHC hiding (compileExpr)
import qualified GHC
import DynFlags
import MonadUtils hiding (MonadIO, liftIO)
import Outputable
import Packages
import Exception

import Eval.EvalError
import Eval.EvalM 
import Display

-- | Loads the file into the evaluator
loadFile :: FilePath -> EvalM ()
loadFile file = do
  setTargets =<< sequence [ guessTarget file Nothing
                          , guessTarget "Helper.hs" Nothing]
  graph <- depanal [] False
  -- output graph
  loaded <- load LoadAllTargets
  when (failed loaded) $ throw LoadingException
  setContext (map (IIModule . moduleName . ms_mod) graph)

-- | Compiles an expression to a @DisplayResult@
compileExpr :: String -> EvalM DisplayResult
compileExpr expr = do
  ty <- exprType expr -- throws exception if doesn't typecheck
  -- output ty
  unsafePerformIO . unsafeCoerce <$> GHC.compileExpr expr


-- | Add a package database to the Ghc monad
#if __GLASGOW_HASKELL_ >= 707  
addPkgDb :: GhcMonad m => FilePath -> m ()
#else
addPkgDb :: (MonadIO m, GhcMonad m) => FilePath -> m ()
#endif
addPkgDb fp = do
  dfs <- getSessionDynFlags
  let pkg  = PkgConfFile fp
  let dfs' = dfs { extraPkgConfs = (pkg:) . extraPkgConfs dfs }
  setSessionDynFlags dfs'
#if __GLASGOW_HASKELL_ >= 707    
  _ <- initPackages dfs'
#else
  _ <- liftIO $ initPackages dfs'
#endif
  return ()

-- | Add a list of package databases to the Ghc monad
-- This should be equivalen to  
-- > addPkgDbs ls = mapM_ addPkgDb ls
-- but it is actaully faster, because it does the package
-- reintialization after adding all the databases
#if __GLASGOW_HASKELL_ >= 707      
addPkgDbs :: GhcMonad m => [FilePath] -> m ()
#else
addPkgDbs :: (MonadIO m, GhcMonad m) => [FilePath] -> m ()
#endif             
addPkgDbs fps = do 
  dfs <- getSessionDynFlags
  let pkgs = map PkgConfFile fps
  let dfs' = dfs { extraPkgConfs = (pkgs ++) . extraPkgConfs dfs }
  setSessionDynFlags dfs'
#if __GLASGOW_HASKELL_ >= 707    
  _ <- initPackages dfs'
#else
  _ <- liftIO $ initPackages dfs'
#endif       
  return ()
  
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
  dfs <- getSessionDynFlags
  let style = defaultUserStyle
      cntx = initSDocContext dfs style
  liftIO $ print $ runSDoc (ppr a) cntx
