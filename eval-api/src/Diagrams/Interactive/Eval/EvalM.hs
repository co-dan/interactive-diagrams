{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Diagrams.Interactive.Eval.EvalM
    (
      -- * Types
      EvalResult
    , EvalM(..)
      -- * Functions
    , run
    , run'
    , liftGhc
    , evalSettings
    ) where

import           Control.Applicative          ((<$>))
import           Control.Monad                (liftM)
import           Control.Monad.Reader         (MonadReader (..), ReaderT (..),
                                               runReaderT)
import           Control.Monad.Trans
import           Data.IORef                   (newIORef, readIORef)
import           DynFlags
import           Exception
import           GHC
#if __GLASGOW_HASKELL__ < 707
import qualified MonadUtils
#endif

import           Diagrams.Interactive.Display
import           Diagrams.Interactive.Eval.EvalSettings
import           Diagrams.Interactive.Eval.Handlers

type EvalResult = Either String DisplayResult

newtype EvalM a = EvalM { unEvalM :: ReaderT EvalSettings Ghc a }
  deriving (Monad,
            MonadReader EvalSettings,
            MonadIO)

-- | Ask specialized to 'EvalSettings'
evalSettings :: EvalM EvalSettings
evalSettings = ask

-- | Runs an EvalM monad action and returns either a result, or an error message
run :: EvalM a -> EvalSettings -> IO (Either String a)
run m set = do
    ref <- newIORef []
    r <- handleException $ run' (liftGhc (initGhc ref (verbLevel set)) >> m) set
    logMsg <- unlines . map show <$> readIORef ref
    case r of
        Left s -> return $ Left $ s ++ "\n" ++ logMsg
        _ -> return r

-- | Runs an EvalM monad action
run' :: EvalM a -> EvalSettings -> IO a
run' m set = runGhc (libDirPath set) (runEvalM m set)

-- | Lift Ghc action to the EvalM monad
liftGhc :: Ghc a -> EvalM a
liftGhc = EvalM . lift


runEvalM :: EvalM a -> EvalSettings -> Ghc a
runEvalM (EvalM act') = runReaderT act'

#if __GLASGOW_HASKELL__ < 707
instance MonadIO Ghc where
    liftIO = MonadUtils.liftIO
#endif

instance ExceptionMonad EvalM where
    gcatch (EvalM act) ctch =
        EvalM $ ReaderT $ \r ->
          runReaderT act r
          `gcatch` (\e -> runReaderT (unEvalM (ctch e)) r)

    gmask callb =
        EvalM $ ReaderT $ \r ->
          gmask $ \ghc_restore -> do
              let eval_restore act =
                      liftGhc $ ghc_restore (runEvalM act r)
              runEvalM (callb eval_restore) r

#if __GLASGOW_HASKELL__ < 707
instance MonadUtils.MonadIO EvalM where
    liftIO = liftIO
#endif

instance HasDynFlags EvalM where
    getDynFlags = liftGhc getDynFlags

instance Functor EvalM where
    fmap = liftM

instance GhcMonad EvalM where
    getSession = liftGhc getSession
    setSession = liftGhc . setSession
