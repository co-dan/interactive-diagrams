{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Various handlers for the GHC API
module Eval.Handlers
    (
      initGhc
    , handleException
    ) where

import Control.Monad          (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (IORef, modifyIORef')

import DynFlags
import Exception
import GHC
import Outputable
import Panic

import Eval.EvalError
import SignalHandlers

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

