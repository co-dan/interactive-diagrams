{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where


import           DsMeta                 (templateHaskellNames)
import           DynFlags
import           GhcMonad
import           HscTypes
import           IfaceEnv               (initNameCache)
import           MonadUtils             (MonadIO (..))
import           Outputable             hiding ((<>))
import           Packages
import           PrelInfo               (wiredInThings)
import           PrelNames              (basicKnownKeyNames)
import           PrimOp                 (allThePrimOps)

import           Data.IORef
import           System.FilePath

import           Compiler.GhcjsPlatform
import           Compiler.Info
import           Compiler.Utils
import qualified Gen2.PrimIface         as Gen2
import           GHCJS


import           Debug.Trace

debug :: Bool
debug = True


main :: IO ()
main = runGhcjsSession Nothing $ do
    traceM "Inside the runGhcjsSession"
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { verbosity = 1 }
    dflags2 <- getSessionDynFlags
    setTargets =<< sequence
        [ guessTarget "test.hs" Nothing ]
    _ <- load LoadAllTargets
    liftIO $ compilationProgressMsg dflags2 "OK"

runGhcjsSession :: Maybe FilePath -> Ghc b -> IO b
runGhcjsSession mbMinusB m = runGhcSession mbMinusB $ do
    initGhcSess
    m

initGhcSess :: Ghc ()
initGhcSess = do
    base <- liftIO ghcjsDataDir
    db1  <- liftIO getGlobalPackageDB
    db2  <- liftIO getUserPackageDB
    dflags <- getSessionDynFlags
    (dflags2,_) <- liftIO $ initPackages $ dflags
                   { extraPkgConfs = const [PkgConfFile db1, PkgConfFile db2] }
    _ <- setSessionDynFlags
         $ setGhcjsPlatform debug [] base
         $ updateWays $ addWay' (WayCustom "js")
         $ setGhcjsSuffixes False dflags2
    fixNameCache
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

-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx

