{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternGuards            #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-|
  Helper functions for the 'EvalM' and 'Ghc' monads
-}
module Diagrams.Interactive.Eval.Helpers
    (
      -- * Compilation and interpretation (of computer programs)
      loadFile
    , compileExpr
      -- * Code queries
    , isUnderIO
    , needsInput
      -- * Auxilary functions for working with the GHC API session/environment
    , addPkgDb
    , addPkgDbs
      -- * Other stuff
    , output
    , allExceptions
    ) where

import Control.Monad                       (when)
import Control.Monad.IO.Class              (MonadIO)
import Unsafe.Coerce                       (unsafeCoerce)

import DynFlags
import Exception
import GHC                                 hiding (compileExpr)
import GhcMonad
import HscMain
import HscTypes
import MonadUtils                          hiding (MonadIO, liftIO)
import Outputable
import Packages                            hiding (display)
import TyCon
import Type
import Util                                (lengthAtLeast)

import Diagrams.Interactive.Display
import Diagrams.Interactive.Eval.EvalError
import Diagrams.Interactive.Eval.EvalM

------------------------------------------------------------
-- Code queries
------------------------------------------------------------

-- | Can the expression be run statically or does it need
-- additional input?
-- NB: IO actions doesn't count seems we can provide almost all of the
-- RealWorld to the use
-- See tests for example behaviour
needsInput :: String -> EvalM Bool
needsInput expr = do
    ty <- peelIO <$> exprType expr
    liftIO $ putStr $ expr ++ " : "
    isFunc ty

-- | See also 'repType' in Types.lhs
isFunc :: Type -> EvalM Bool
isFunc t = do
    ioTyC <- getIOTyCon
    let t' = go initRecTc [ioTyC] t
    output t'
    return $ isFunTy t'
  where
    go :: RecTcChecker -> [TyCon] -> Type -> Type
    go rec_nts ignore ty    
      | Just ty' <- coreView ty
      = go rec_nts ignore ty'

      | Just (_, ty') <- splitForAllTy_maybe ty
      = go rec_nts ignore ty'

      | Just (tc, tys) <- splitTyConApp_maybe ty
      , isNewTyCon tc
      , not (elem tc ignore)  
      , tys `lengthAtLeast` tyConArity tc
	  , Just rec_nts' <- checkRecTc rec_nts tc   -- See Note [Expanding newtypes] in TyCon
      = go rec_nts' ignore (newTyConInstRhs tc tys)

      | otherwise = ty

    -- any isFunTy . flattenRepType . repType


--------------------------------------------------

peelIO :: Type -> Type
peelIO ty = ty

getIOTyCon :: EvalM TyCon
getIOTyCon = tyConAppTyCon <$> exprType "(return ()) :: IO ()"

-- | Is the expression under the IO Monad?
isUnderIO :: String -> EvalM Bool
isUnderIO expr = do
    ty <- exprType expr
    let splitted = tyConAppTyCon_maybe ty
    case splitted of
        Nothing -> return False
        Just tyC -> do
            ioTyC <- getIOTyCon
            return $ tyC == ioTyC


------------------------------------------------------------
-- Compilation and interpretation
------------------------------------------------------------

displayImport :: InteractiveImport
displayImport = IIDecl . simpleImportDecl $ mkModuleName "Diagrams.Interactive.Display"

-- | Loads the file into the evaluator
loadFile :: FilePath -> EvalM ()
loadFile file = do
    setTargets =<< sequence [guessTarget file Nothing]
    graph <- depanal [] False
    -- output graph
    loaded <- load LoadAllTargets
    when (failed loaded) $ throw LoadingException
    setContext $ displayImport:(map (IIModule . ms_mod_name) graph)

-- | Compiles an expression to a @StaticResult@
compileExpr :: String -> EvalM StaticResult
compileExpr expr = do
    -- ty <- exprType expr -- throws exception if doesn't typecheck
    -- output ty

    sess <- getSession
    Just (vars, hval, fix_env) <- liftIO $ hscStmt sess expr -- ("let __cmCompileExpr="++expr)
    updateFixityEnv fix_env
    hvals <- liftIO ((Left <$> hval)
                     `catch` allExceptions)
    case (vars,hvals) of
        ([_], Left [hv]) -> return . unsafeCoerce $ hv
        -- if underIO
        -- then liftIO . unsafeCoerce $ hv
        -- else return . unsafeCoerce $ hv
        ([_], Right exn) -> return (display exn)
        _                -> panic "compileExpr"


------------------------------------------------------------
-- Auxilary functions for working with the
--    GHC API session/environment
------------------------------------------------------------

-- | Update fixity environment in the current interactive context.
updateFixityEnv :: GhcMonad m => FixityEnv -> m ()
updateFixityEnv fix_env = do
    hsc_env <- getSession
    let ic = hsc_IC hsc_env
    setSession $ hsc_env { hsc_IC = ic { ic_fix_env = fix_env } }

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
    _ <- setSessionDynFlags dfs'
#if __GLASGOW_HASKELL_ >= 707
    _ <- initPackages dfs'
#else
    _ <- liftIO $ initPackages dfs'
#endif
    return ()

-- | Add a list of package databases to the Ghc monad
-- This should be equivalen to
--
-- > addPkgDbs ls = mapM_ addPkgDb ls
--
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
    _ <- setSessionDynFlags dfs'
#if __GLASGOW_HASKELL_ >= 707
    _ <- initPackages dfs'
#else
    _ <- liftIO $ initPackages dfs'
#endif
    return ()


------------------------------------------------------------
-- Other stuff
------------------------------------------------------------

-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx

allExceptions :: Monad m => SomeException -> m (Either a SomeException)
allExceptions = return . Right
