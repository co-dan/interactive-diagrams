{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module Main where

import Control.Monad                       (when, void)
import Control.Monad.IO.Class              (MonadIO)
import Unsafe.Coerce                       (unsafeCoerce)
import Data.Monoid
import Data.List

import Distribution.Package (PackageName (..))

import Bag
import DynFlags hiding (PackageName(..))
import DriverPipeline hiding (PackageName(..))
import Exception hiding (PackageName(..))
import GHC                                 hiding (compileExpr, PackageName(..))
import GHC.Paths hiding (PackageName(..))
import GhcMonad  hiding (PackageName(..))
-- import HscMain hiding (PackageName(..))
import UniqFM (eltsUFM)
import Module hiding (PackageName(..))
import HscTypes hiding (PackageName(..))
import OccName hiding (PackageName(..))
import RdrName hiding (PackageName(..))
import HsBinds hiding (PackageName(..))
import MonadUtils                          hiding (MonadIO, liftIO)
import Outputable                          hiding ((<>))
import Packages                            hiding (display, PackageName(..))
import TyCon hiding (PackageName(..))
import Type hiding (PackageName(..))
import Util                                (lengthAtLeast)

import GHCJS hiding (compileExpr, PackageName(..))
import Compiler.GhcjsPlatform hiding (PackageName(..))
import Compiler.GhcjsHooks hiding (PackageName(..))
import Compiler.Variants hiding (PackageName(..))

import Diagrams.Interactive.Display
import Diagrams.Interactive.Eval.EvalError
import Diagrams.Interactive.Eval.EvalSettings
import Diagrams.Interactive.Eval.EvalM
import Diagrams.Interactive.Eval.SourceMod
import Diagrams.Interactive.Eval.Helpers 

main = runGhcjsSession Nothing True $ do
    let tmpDirPath = "/tmp"
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { verbosity = 1
                                , objectDir = Just tmpDirPath
                                , hiDir     = Just tmpDirPath
                                , dumpDir   = Just tmpDirPath
                                , stubDir   = Just tmpDirPath
                                --, hscTarget = HscInterpreted
                                }
    trgt <- guessTarget "testjs.hs" Nothing
    setTargets [trgt]
    graph <- depanal [] False
    

    let modSum = head graph
    output modSum
    dflags'' <- getSessionDynFlags
    (_,dep'') <- liftIO $ initPackages dflags''
    output dep''
    loaded <- load LoadAllTargets
    --loaded <- load (LoadDependenciesOf (ms_mod_name modSum))
    when (failed loaded) $ throw LoadingException
    parsedMod <- parseModule modSum
    output (pm_mod_summary parsedMod)
    let src = pm_parsed_source parsedMod
    let (L srcspan hsmod) = src
    let hsmod' = modifyModule injectRender hsmod
    output $ hsmod' 
    typecheckedMod <- typecheckModule $ parsedMod
                      { pm_parsed_source = L srcspan hsmod' }
    output $ tm_typechecked_source typecheckedMod
    liftIO $ putStrLn "loading mod..."
    mod <- loadModule typecheckedMod
    let modSum' = pm_mod_summary (tm_parsed_module typecheckedMod)
    output modSum'

    hsc_env <- getSession
    dflags2  <- getSessionDynFlags
    let hpt = hsc_HPT hsc_env :: HomePackageTable
    let home_mod_infos = eltsUFM hpt
    let pkg_deps  = concatMap (map fst . dep_pkgs . mi_deps . hm_iface) home_mod_infos
    let pkg_deps' | any isInteractivePackage pkg_deps = pkg_deps
                  | otherwise = displayInteractivePackage dflags2 : pkg_deps
    output pkg_deps'
    liftIO $ ghcjsLinkBinary True [] dflags2 ["/tmp/Main.js_o"] pkg_deps'

    -- modifySession $ \h -> h { hsc_mod_graph = modSum':(tail graph) }
    -- -- hsc_env <- getSession
    -- -- dflags2  <- getSessionDynFlags
    -- -- linkresult <- liftIO $ link (ghcLink dflags2) dflags True 
    -- --               (hsc_HPT hsc_env)

    -- -- output linkresult
    -- liftIO $ putStrLn "load.."
    -- void (load LoadAllTargets) `gcatch` \(e::SomeException) -> liftIO $ print e
    -- liftIO $ putStrLn "loaded"

isInteractivePackage :: PackageId -> Bool
isInteractivePackage pkgId = "display-interactive-" `isPrefixOf` packageIdString pkgId

displayInteractivePackage :: DynFlags -> PackageId    
displayInteractivePackage dflags =
  case prims of
    (x:_) -> x
    _     -> error "Cannot find display-interactive"
  where
    prims = reverse . sort $ filter isInteractivePackage pkgIds
    pkgIds = map packageConfigId . eltsUFM . pkgIdMap . pkgState $ dflags

--0 ((PackageName "display-interactive"==) . pkgName
