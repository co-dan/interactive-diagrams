{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module Main where

import Control.Monad                       (when)
import Data.List
import Data.Default

import Distribution.Package (PackageName (..))
import DynFlags
import Exception
import GHC                                 hiding (compileExpr)
import GHC.Paths
import GhcMonad  
import UniqFM (eltsUFM)
import Module
import HscTypes
import Outputable                          hiding ((<>))
import Packages                            hiding (display)

import GHCJS hiding (compileExpr(..))
import Compiler.GhcjsPlatform 
import Compiler.GhcjsHooks 
import Compiler.Variants 

import Diagrams.Interactive.Display
import Diagrams.Interactive.Eval.EvalSettings hiding (tmpDirPath)
import Diagrams.Interactive.Eval.SourceMod
import Diagrams.Interactive.Eval.EvalM
import Diagrams.Interactive.Eval.Helpers
import Diagrams.Interactive.Eval.Handlers 

tmpDirPath = "/tmp"
fp = "testjs.hs"
main = flip run def $ liftGhc $ do
    mainact
    mainact
    
mainact = do
    initGhcJs True
    do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { verbosity = 1
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
        when (moduleNameString (ms_mod_name modSum) /= "Main") $
            panic "DynamicResult only allowed in `Main' module"
        dflags'' <- getSessionDynFlags
        (_,dep'') <- liftIO $ initPackages dflags''
        parsedMod <- parseModule modSum
        liftIO $ print "****************************** parsed"
        let src = pm_parsed_source parsedMod
        let (L srcspan hsmod) = src
        let hsmod' = modifyModule injectRender hsmod
        output hsmod'
        typecheckedMod <- typecheckModule $ parsedMod
                          { pm_parsed_source = L srcspan hsmod' }
        -- This will load the module and produce the obj file
        output (tm_typechecked_source typecheckedMod)
        mod <- loadModule typecheckedMod
        let modSum' = pm_mod_summary (tm_parsed_module typecheckedMod)

        -- Linking stuff
        hsc_env <- getSession
        dflags2  <- getSessionDynFlags
        let hpt = hsc_HPT hsc_env :: HomePackageTable
        let home_mod_infos = eltsUFM hpt
        let pkg_deps  = concatMap ( map fst . dep_pkgs
                                  . mi_deps . hm_iface )
                        home_mod_infos
        let pkg_deps' | any isInteractivePackage pkg_deps = pkg_deps
                      | otherwise = displayInteractivePackage dflags2 : pkg_deps
        liftIO $ linkBinary dflags2 pkg_deps' ["/tmp/Main.js_o"] "out.jsexe"
        liftIO $ print "/link"

linkBinary :: DynFlags -> [PackageId] -> [FilePath] -> FilePath -> IO ()
linkBinary dflags pkg_deps targets out =
    print =<< variantLink gen2Variant dflags True out [] deps targets [] isRoot
  where
    isRoot = const True
    deps   = map (\pkg -> (pkg, packageLibPaths pkg)) pkg_deps'
    pidMap   = pkgIdMap (pkgState dflags)
    packageLibPaths :: PackageId -> [FilePath]
    packageLibPaths pkg = maybe [] libraryDirs (lookupPackage pidMap pkg)
    -- make sure we link ghcjs-prim even when it's not a dependency
    pkg_deps' | any isGhcjsPrimPackage pkg_deps = pkg_deps
              | otherwise                       = ghcjsPrimPackage dflags : pkg_deps


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
