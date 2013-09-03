{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternGuards            #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-|
  Helper functions for the 'EvalM' and 'Ghc' monads
-}
module Diagrams.Interactive.Eval.Helpers
    (
      -- * Compilation and interpretation (of computer programs)
      loadFile
    , compileExpr
    , compileToJS
      -- * Code queries
    , isUnderIO
    , needsInput
      -- * Auxilary functions for working with the GHC API session/environment
    , addPkgDb
    , addPkgDbs
      -- * Other stuff
    , output
    , allExceptions
    , injectRender
    ) where

import Control.Monad                       (when, void)
import Control.Monad.IO.Class              (MonadIO)
import Unsafe.Coerce                       (unsafeCoerce)
import Data.Monoid

import Bag
import DynFlags
import DriverPipeline
import Exception
import GHC                                 hiding (compileExpr)
import GhcMonad
import HscMain
import HscTypes
import OccName
import RdrName
import HsBinds
import MonadUtils                          hiding (MonadIO, liftIO)
import Outputable                          hiding ((<>))
import Packages                            hiding (display)
import TyCon
import Type
import Util                                (lengthAtLeast)

import GHCJS hiding (compileExpr)
import Compiler.GhcjsPlatform
import Compiler.Variants

import Diagrams.Interactive.Display
import Diagrams.Interactive.Eval.EvalError
import Diagrams.Interactive.Eval.EvalSettings
import Diagrams.Interactive.Eval.EvalM
import Diagrams.Interactive.Eval.SourceMod

------------------------------------------------------------
-- Code queries
------------------------------------------------------------

-- | Can the expression be run statically or does it need
-- additional input?
-- NB: IO actions doesn't count seems we can provide almost all of the
-- RealWorld to the use
-- See tests for example behaviour
needsInput :: String -> EvalM Bool
needsInput expr = isFunc =<< exprType expr

-- | See also 'repType' in Types.lhs
isFunc :: Type -> EvalM Bool
isFunc t = do
    ioTyC <- getIOTyCon
    t' <- peelIO t
    return $ isFunTy $ go initRecTc [ioTyC] t'
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

peelIO :: Type -> EvalM Type
peelIO ty = do
    let splitted = splitTyConApp_maybe ty
    case splitted of
        Nothing -> return ty
        Just (tyC, tys) -> do
            ioTyC <- getIOTyCon
            if (tyC == ioTyC)
              then return (head tys)
              else return ty     
    
getIOTyCon :: EvalM TyCon
getIOTyCon = tyConAppTyCon <$> exprType "(return ()) :: IO ()"

-- | Is the expression under the IO Monad?
isUnderIO :: Type -> EvalM Bool
isUnderIO ty = do
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
loadFile :: GhcMonad m => FilePath -> m ()
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


compileToJS :: FilePath -> EvalM DynamicResult
compileToJS fp = do
    EvalSettings{..} <- evalSettings
    liftIO $ runGhcjsSession Nothing True $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { verbosity = verbLevel
                                    , objectDir = Just tmpDirPath
                                    , hiDir     = Just tmpDirPath
                                    , dumpDir   = Just tmpDirPath
                                    , stubDir   = Just tmpDirPath
                                    --, hscTarget = HscInterpreted
                                    }
        trgt <- guessTarget fp Nothing
        setTargets [trgt]
        graph <- depanal [] False
        let modSum = head graph
        output graph
        -- loaded <- load LoadAllTargets
        loaded <- load (LoadDependenciesOf (ms_mod_name modSum))
        when (failed loaded) $ throw LoadingException
        parsedMod <- parseModule modSum
        let src = pm_parsed_source parsedMod
        let (L srcspan hsmod) = src
        let hsmod' = modifyModule injectRender hsmod
        output $ hsmod' 
        typecheckedMod <- typecheckModule $ parsedMod
                          { pm_parsed_source = L srcspan hsmod' }
        output $ tm_typechecked_source typecheckedMod
        liftIO $ putStrLn "loading mod..."
        -- mod <- loadModule typecheckedMod
        let modSum' = pm_mod_summary (tm_parsed_module typecheckedMod)
        modifySession $ \h -> h { hsc_mod_graph = modSum':(tail graph) }
        -- hsc_env <- getSession
        -- dflags2  <- getSessionDynFlags
        -- linkresult <- liftIO $ link (ghcLink dflags2) dflags True 
        --               (hsc_HPT hsc_env)
        
        -- output linkresult
        liftIO $ putStrLn "load.."
        void (load LoadAllTargets) `gcatch` \(e::SomeException) -> liftIO $ print e
        liftIO $ putStrLn "loaded"
        return (DynamicResult "WOAH")


injectRender :: SourceMod ()
injectRender = do
    addImportSimple dclass
    maybety <- removeSig maindef
    replaceDefinition maindef (wrapRender maybety)
  where
    dclass  = "Diagrams.Interactive.Display.Dynamic.Class"
    maindef = mkVarOcc "example"

wrapRender :: Maybe (HsType RdrName) -> HsBind RdrName -> HsBind RdrName
wrapRender ty f@(FunBind{..})
          = f { fun_matches = fun_matches {
                     mg_alts = [L l (Match [] Nothing grhs)]
                     } }
  where
    grhs = GRHSs { grhssLocalBinds = binds
                 , grhssGRHSs = [L l $ GRHS [] (L l rhs) ] }
    
    dclass  = "Diagrams.Interactive.Display.Dynamic.Class"
    newName = mkRdrUnqual (mkVarOcc "oldexample")
    renderF = mkRdrQual (mkModuleName dclass) (mkVarOcc "runRenderTest")
    renderV = HsVar renderF
    l = getLoc fun_id
    rhs :: HsExpr RdrName    
    rhs = HsApp (L l renderV) (L l (HsVar newName))
    -- rhs = HsLet binds (L l $ HsApp (L l renderV) (L l (HsVar newName)))
    binds :: HsLocalBinds RdrName
    binds = HsValBinds $ ValBindsIn (unitBag (L l newF)) sigs
    sigs = case ty of
        Nothing  -> []
        Just ty' -> [L l $ TypeSig [L l newName] (L l ty')]
    newF = f { fun_id = L l newName }
wrapRender _ x = x

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
