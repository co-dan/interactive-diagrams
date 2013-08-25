{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
module Main where


import DynFlags
import GhcMonad
import MonadUtils         (MonadIO (..))
import Outputable         hiding ((<>))

import Compiler.Utils
import GHCJS

import Debug.Trace
import System.Environment (getArgs)

debug :: Bool
debug = True


main :: IO ()
main = do
    args <- getArgs
    let fname = case args of
            (fn:_) -> fn
            []     -> "test.hs"
    runGhcjsSession Nothing debug $ do
        traceM "Inside the runGhcjsSession"
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { verbosity = 1 }
        dflags2 <- getSessionDynFlags
        setTargets =<< sequence
            [ guessTarget fname Nothing ]
        _ <- load LoadAllTargets
        liftIO $ compilationProgressMsg dflags2 "OK"


-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx
