{-# LANGUAGE CPP,
             TypeFamilies,
             ScopedTypeVariables,
             TupleSections,
             OverloadedStrings #-}
module Main where

import           Config (cProjectVersion, cDYNAMIC_GHC_PROGRAMS)
import           GHC
import           Hooks
import           HscMain
import           TidyPgm (tidyProgram)
import           CoreToStg (coreToStg)
import           SimplStg (stg2stg)
import           UniqFM (eltsUFM)
import           DynFlags
import           Platform
import   Outputable hiding ((<>))
import           ErrUtils (fatalErrorMsg'')
import           CorePrep (corePrepPgm)
import           DriverPhases (HscSource, Phase(..),
                               isHaskellSrcFilename, isHaskellUserSrcFilename,
                               isSourceFilename, startPhase)
import           DriverPipeline
import           DriverMkDepend ( doMkDependHS )
import           DsMeta (templateHaskellNames)
import           Exception
import           HscTypes (CgGuts(..), HscEnv(..), Dependencies(..),
                           NameCache (..), isBootSummary,
                           FindResult(..),
                           mkSOName, mkHsSOName, typeEnvTyCons )
import           IfaceEnv (initNameCache)
import           LoadIface
import           Outputable (showPpr)
import           Panic
import           Module
import           PrelInfo (wiredInThings)
import           PrelNames (basicKnownKeyNames)
import           PrimOp (allThePrimOps)
import           SysTools (touch, LinkDynLibHook(..))
import           Packages
import           MkIface
import           GhcMonad
import           Digraph
import           Binary (fingerprintBinMem, openBinMem, put_)
import           Constants (hiVersion)
import           TcRnMonad (initIfaceCheck)
import           Util (looksLikeModuleName)
import           Outputable hiding ((<>))
import           MonadUtils (MonadIO(..))
import qualified SysTools
import           Linker (locateLib', LocateLibHook(..), LibrarySpec(..))
import           Control.Applicative
import qualified Control.Exception as Ex
import           Control.Monad

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8  as C8
import           Data.Char (toLower)
import           Data.IORef (modifyIORef, writeIORef)
import           Data.List (isSuffixOf, isPrefixOf, tails, partition, nub,
                            intercalate, foldl', isInfixOf, sort)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Encoding as T

import           Distribution.Package (PackageName(..), PackageIdentifier(..))

import           Options.Applicative
import           Options.Applicative.Types
import           Options.Applicative.Builder.Internal

import           System.Directory (createDirectoryIfMissing, getAppUserDataDirectory,
                                   doesFileExist, copyFile)
import           System.Environment (getArgs, getEnv)
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process (rawSystem)

import           Compiler.Info
import           Compiler.Variants
import           Compiler.GhcjsHooks

import qualified Gen2.Utils     as Gen2
import qualified Gen2.Generator as Gen2
import qualified Gen2.Linker    as Gen2
import qualified Gen2.Rts       as Gen2
import qualified Gen2.PrimIface as Gen2
import qualified Gen2.Foreign   as Gen2
import qualified Gen2.Object    as Object

import           Finder (findImportedModule, cannotFindInterface)

import Debug.Trace 

data GhcjsSettings = GhcjsSettings { gsNativeExecutables :: Bool
                                   , gsNoNative          :: Bool
                                   , gsNoJSExecutables   :: Bool
                                   , gsLogCommandLine    :: Maybe FilePath
                                   , gsGhc               :: Maybe FilePath
                                   , gsDebug             :: Bool
                                   } deriving (Eq, Show)

instance Monoid GhcjsSettings where
  mempty = GhcjsSettings False False False Nothing Nothing False
  mappend (GhcjsSettings ne1 nn1 nj1 lc1 gh1 dbg1)
          (GhcjsSettings ne2 nn2 nj2 lc2 gh2 dbg2) =
          GhcjsSettings (ne1 || ne2) (nn1 || nn2) (nj1 || nj2) (lc1 <> lc2) (gh1 <> gh2) (dbg1 || dbg2)

base = "/home/vagrant/.cabal/share/i386-linux-ghc-7.7.20130815/ghcjs-0.1.0"
main = do
    libDir <- getGlobalPackageBase
    db1 <- getGlobalPackageDB
    db2 <- getUserPackageDB
    traceShow libDir $ return ()
    
    defaultErrorHandler
        defaultFatalMessager
        defaultFlushOut $
        runGhc (Just libDir) $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags $ dflags
                { extraPkgConfs = const [PkgConfFile db1, PkgConfFile db2]
                , includePaths  = (base ++ "/include"):includePaths dflags}

            dflags2 <- getSessionDynFlags
            (dflags3, pkgs) <- liftIO (initPackages dflags2)
            _ <- setSessionDynFlags $
                 setGhcjsPlatform mempty [] base $
                 updateWays $ addWay' (WayCustom "js") $
                 setGhcjsSuffixes False dflags3

            fixNameCache

            dflags4 <- getSessionDynFlags
            
            liftIO $ putStrLn "generating core"
            cm <- compileToCoreModule "test.hs"
--             output cm
--             hsc <- getSession
--             let (mod, binds, tyenv) = (cm_module cm, cm_binds cm, cm_types cm)
--             let tycons = typeEnvTyCons tyenv
--             core_binds <- liftIO $ corePrepPgm dflags4 hsc binds tycons
--             liftIO $ putStrLn "generating STG"
--             stg <- liftIO $ coreToStg dflags4 mod core_binds
-- -- --            stgs <- liftIO $ coreToStg dflags4 mod binds
--             (stg', _) <- liftIO $ stg2stg dflags4 mod stg
--             liftIO $ putStrLn "generating JS"            
--             let js = Gen2.generate False dflags4 stg' mod
--             liftIO $ B.writeFile "test.js" js

-- -- --            setTargets =<< sequence [guessTarget "test.hs" Nothing]
-- -- --            load LoadAllTargets
            return ()


setGhcjsSuffixes :: Bool     -- oneshot option, -c
                 -> DynFlags
                 -> DynFlags
setGhcjsSuffixes oneshot df = df
    { objectSuf     = mkGhcjsSuf (objectSuf df)
    , dynObjectSuf  = mkGhcjsSuf (dynObjectSuf df)
    , hiSuf         = mkGhcjsSuf (hiSuf df)
    , dynHiSuf      = mkGhcjsSuf (dynHiSuf df)
    , outputFile    = fmap mkGhcjsOutput (outputFile df)
    , dynOutputFile = fmap mkGhcjsOutput (dynOutputFile df)
    , outputHi      = fmap mkGhcjsOutput (outputHi df)
    , ghcLink       = if oneshot then NoLink else ghcLink df
    }

mkGhcjsOutput :: String -> String
mkGhcjsOutput "" = ""
mkGhcjsOutput file
  | ext == ".hi"     = replaceExtension file ".js_hi"
  | ext == ".o"      = replaceExtension file ".js_o"
  | ext == ".dyn_hi" = replaceExtension file ".js_dyn_hi"
  | ext == ".dyn_o"  = replaceExtension file ".js_dyn_o"
  | otherwise        = replaceExtension file (".js_" ++ drop 1 ext)
  where
    ext = takeExtension file
mkGhcjsSuf :: String -> String
mkGhcjsSuf "o"      = "js_o"
mkGhcjsSuf "hi"     = "js_hi"
mkGhcjsSuf "dyn_o"  = "js_dyn_o"
mkGhcjsSuf "dyn_hi" = "js_dyn_hi"
mkGhcjsSuf xs       = "js_" ++ xs -- is this correct?


-- replace primops in the name cache so that we get our correctly typed primops
fixNameCache :: GhcMonad m => m ()
fixNameCache = do
  sess <- getSession
  liftIO $ modifyIORef (hsc_NC sess) $ \(NameCache u _) ->
    (initNameCache u knownNames)
    where
      knownNames = map getName (filter (not.isPrimOp) wiredInThings) ++
                      basicKnownKeyNames ++
                      templateHaskellNames ++
                      map (getName . AnId . Gen2.mkGhcjsPrimOpId) allThePrimOps
      isPrimOp (AnId i) = isPrimOpId i
      isPrimOp _        = False

-- -- | configure the GHC API for building 32 bit JavaScript code
setGhcjsPlatform :: GhcjsSettings -> [FilePath] -> FilePath -> DynFlags -> DynFlags
setGhcjsPlatform set js_objs basePath df
  = addPlatformDefines basePath
      $ setDfOpts
      $ installGhcjsHooks (gsDebug set) js_objs
      $ installDriverHooks (gsDebug set)
      $ df { settings = settings' }
  where
    settings' = (settings df) { sTargetPlatform    = ghcjsPlatform
                              , sPlatformConstants = ghcjsPlatformConstants
                              }
    ghcjsPlatform = (sTargetPlatform (settings df))
       { platformArch     = ArchJavaScript
       , platformWordSize = 4
       }
    ghcjsPlatformConstants = (sPlatformConstants (settings df))
       { pc_WORD_SIZE       = 4
       , pc_DOUBLE_SIZE     = 8
       , pc_CINT_SIZE       = 4
       , pc_CLONG_SIZE      = 4
       , pc_CLONG_LONG_SIZE = 8
       , pc_WORDS_BIGENDIAN = False
       }

setOpt = gopt_set
unsetOpt = gopt_unset

-- add some configs
setDfOpts :: DynFlags -> DynFlags
setDfOpts df = foldl' setOpt (foldl' unsetOpt df unsetList) setList
  where
    setList = []
    unsetList = [Opt_SplitObjs]


--------------------------------------------------

-- ghcjs builds for a strange platform: like 32 bit
-- instead of letting autoconf doing the defines, we override them here
-- and try to get our own includes included instead of the library ones
addPlatformDefines :: FilePath -> DynFlags -> DynFlags
addPlatformDefines baseDir df = df { settings = settings1
                                   , includePaths = includeDir : includePaths df
                                   }
  where
    includeDir = baseDir ++ "/include"
    settings0 = settings df
    settings1 = settings0 { sOpt_P = ("-I" ++ includeDir) : map ("-D"++) defs ++ sOpt_P settings0 }
    defs = [ "__GHCJS__"
           , "__GHCAUTOCONF_H__=1"
           , "__GHCCONFIG_H__=1"
           , "SIZEOF_CHAR=1"
           , "ALIGNMENT_CHAR=1"
           , "SIZEOF_UNSIGNED_CHAR=1"
           , "ALIGNMENT_UNSIGNED_CHAR=1"
           , "SIZEOF_SHORT=2"
           , "ALIGNMENT_SHORT=2"
           , "SIZEOF_UNSIGNED_SHORT=2"
           , "ALIGNMENT_UNSIGNED_SHORT=2"
           , "SIZEOF_INT=4"
           , "ALIGNMENT_INT=4"
           , "SIZEOF_UNSIGNED_INT=4"
           , "ALIGNMENT_UNSIGNED_INT=4"
           , "SIZEOF_LONG=4"
           , "ALIGNMENT_LONG=4"
           , "SIZEOF_UNSIGNED_LONG=4"
           , "ALIGNMENT_UNSIGNED_LONG=4"
           , "HAVE_LONG_LONG=1"
           , "SIZEOF_LONG_LONG=8"
           , "ALIGNMENT_LONG_LONG=8"
           , "SIZEOF_UNSIGNED_LONG_LONG=8"
           , "ALIGNMENT_UNSIGNED_LONG_LONG=8"
           , "SIZEOF_VOID_P=4"
           , "ALIGNMENT_VOID_P=4"
           , "SIZEOF_DOUBLE=8"
           , "ALIGNMENT_DOUBLE=8"
           , "SIZEOF_FLOAT=4"
           , "ALIGNMENT_FLOAT=4"
           ]


-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx

