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

import           DsMeta                              (templateHaskellNames)
import           DynFlags
import           Exception
import           GHC
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
    _ <- setSessionDynFlags dfs2
    return ()

initGhcJs :: Bool -> Ghc ()
initGhcJs debug = do
    base <- liftIO ghcjsDataDir
    dflags <- getSessionDynFlags
    (dflags2,_) <- liftIO $ initPackages dflags
    _ <- setSessionDynFlags
         $ setGhcjsPlatform debug [] base
         $ updateWays $ addWay' (WayCustom "js")
         $ setGhcjsSuffixes False dflags2
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

