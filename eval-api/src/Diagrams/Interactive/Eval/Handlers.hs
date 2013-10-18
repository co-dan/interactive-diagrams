{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Various handlers for the GHC API
module Diagrams.Interactive.Eval.Handlers
    (
      initGhc
    , initGhcJs
    , handleException
    ) where

import           Control.Monad                       (liftM)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.IORef                          (IORef, modifyIORef')
import           Data.List                           (foldl')

import           DsMeta                              (templateHaskellNames)
import           DynFlags
import           Exception
import           GHC
import           GhcMonad
import           HscTypes
import           IfaceEnv                            (initNameCache)
import           Outputable
import           Packages                            (initPackages)
import           Panic
import           PrelInfo                            (wiredInThings)
import           PrelNames                           (basicKnownKeyNames)
import           PrimOp                              (allThePrimOps)

import           Compiler.GhcjsPlatform
import           Compiler.Info
import           Compiler.Variants
import qualified Gen2.PrimIface                      as Gen2
import           GHCJS

import           Diagrams.Interactive.Eval.EvalError
import           SignalHandlers

-- | Inits the GHC API, sets the mode and the log handler
initGhc :: IORef [EvalError] -> Int -> Ghc ()
initGhc ref vb = do
    dfs <- getSessionDynFlags
    let dfs2 = dfs  { hscTarget = HscInterpreted
                    , ghcLink = LinkInMemory
                    , log_action = logHandler ref
                    , verbosity  = vb
                    }
    let dfs3 = foldl' wopt_set dfs2 minusWallOpts
    _ <- setSessionDynFlags dfs3
    return ()

addGhcjsDB :: DynFlags -> IO DynFlags
addGhcjsDB dfs = do
    let pkg  = PkgConfFile "/home/vagrant/.ghcjs/i386-linux-0.1.0-7.7.20130922/package.conf.d"
    let pkg2  = PkgConfFile "/home/vagrant/.ghcjs/i386-linux-0.1.0-7.7.20130922/lib/package.conf.d"
    let dfs' = dfs { extraPkgConfs = const [pkg, pkg2] }
    return dfs'

-- | This resets the session
initGhcJs :: Bool -> Ghc ()
initGhcJs debug = do
    let base1 = "/home/vagrant/.ghcjs/i386-linux-0.1.0-7.7.20130922/"
    -- libDir <- liftIO $ getGlobalPackageBase
    initGhcMonad (Just base1)
    dflags <- getSessionDynFlags
    dflags2 <- liftIO $ addGhcjsDB dflags
    (dflags3,_) <- liftIO $ initPackages dflags2
    _ <- setSessionDynFlags
         $ setGhcjsPlatform debug [] base1
         $ updateWays $ addWay' (WayCustom "js")
         $ setGhcjsSuffixes False
         $ dflags3 { ghcLink   = LinkBinary
                   , hscTarget = HscAsm
                   }
    fixNameCache

fixNameCache :: GhcMonad m => m ()
fixNameCache = do
  sess <- getSession
  liftIO $ modifyIORef' (hsc_NC sess) $ \(NameCache u _) ->
    (initNameCache u knownNames)
    where
      knownNames = map getName (filter (not.isPrimOp) wiredInThings) ++
                      basicKnownKeyNames ++
                      templateHaskellNames ++
                      map (getName . AnId . Gen2.mkGhcjsPrimOpId) allThePrimOps
      isPrimOp (AnId i) = isPrimOpId i
      isPrimOp _        = False

-- | A log handler for GHC API. Saves the errors and warnings in an 'IORef'
-- LogAction == DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logHandler :: IORef [EvalError] -> LogAction
logHandler ref dflags severity srcSpan style msg =
    case srcSpan of
        RealSrcSpan sp -> do
            modifyIORef' ref (++ [err sp])
        UnhelpfulSpan _ -> return ()
  where
    err sp = EvalError severity msg' (srcPos sp)
    cntx   = initSDocContext dflags style
    msg'   = show (runSDoc msg cntx)


-- | Exception handler for GHC API.
-- Catches all exceptions and restores handlers.
handleException :: (ExceptionMonad m, MonadIO m)
                   => m a -> m (Either String a)
handleException m =
    ghandle (\(ex :: SomeException) -> return (Left (showException ex))) $
    handleGhcException (\ge -> return (Left (showGhcException ge ""))) $
    flip gfinally (liftIO restoreHandlers) $
    liftM Right m



--------------------------------------------------
-- Bits copied from GHC
-- XXX: it's pain in the ass to maintain this?

standardWarnings :: [WarningFlag]
standardWarnings
    = [ Opt_WarnOverlappingPatterns,
        Opt_WarnWarningsDeprecations,
        Opt_WarnDeprecatedFlags,
        Opt_WarnUnrecognisedPragmas,
        Opt_WarnPointlessPragmas,
        Opt_WarnDuplicateConstraints,
        Opt_WarnDuplicateExports,
        Opt_WarnOverflowedLiterals,
        Opt_WarnEmptyEnumerations,
        Opt_WarnMissingFields,
        Opt_WarnMissingMethods,
        Opt_WarnLazyUnliftedBindings,
        Opt_WarnWrongDoBind,
        Opt_WarnUnsupportedCallingConventions,
        Opt_WarnDodgyForeignImports,
        Opt_WarnInlineRuleShadowing,
        Opt_WarnAlternativeLayoutRuleTransitional,
        Opt_WarnUnsupportedLlvmVersion
      ]

minusWOpts :: [WarningFlag]
-- Things you get with -W
minusWOpts
    = standardWarnings ++
      [ Opt_WarnUnusedBinds,
        Opt_WarnUnusedMatches,
        Opt_WarnUnusedImports,
        Opt_WarnIncompletePatterns,
        Opt_WarnDodgyExports,
        Opt_WarnDodgyImports
      ]

minusWallOpts :: [WarningFlag]
-- Things you get with -Wall
minusWallOpts
    = minusWOpts ++
      [ Opt_WarnTypeDefaults,
        Opt_WarnNameShadowing,
        Opt_WarnMissingSigs,
        Opt_WarnHiShadows,
        Opt_WarnOrphans,
        Opt_WarnUnusedDoBind
      ]
