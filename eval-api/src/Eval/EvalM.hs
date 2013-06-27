{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, CPP #-}
module Eval.EvalM where

import Control.Monad (liftM)
import Control.Monad.Trans
import Control.Monad.Reader (ReaderT(..), MonadReader(..),
                             runReaderT)
import GHC
import DynFlags
import Exception
import qualified MonadUtils

import Eval.EvalSettings
import Display
  
type EvalResult = Either String DisplayResult
  
newtype EvalM a = EvalM { unEvalM :: ReaderT EvalSettings Ghc a }
  deriving (Monad,
            MonadReader EvalSettings,
            MonadIO)

liftEvalM :: Ghc a -> EvalM a
liftEvalM = EvalM . lift

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
              liftEvalM $ ghc_restore (runEvalM act r)
        runEvalM (callb eval_restore) r

#if __GLASGOW_HASKELL__ < 707 
instance MonadUtils.MonadIO EvalM where
  liftIO = liftIO
#endif

instance HasDynFlags EvalM where
  getDynFlags = liftEvalM getDynFlags

instance Functor EvalM where
  fmap = liftM
  
instance GhcMonad EvalM where
  getSession = liftEvalM getSession
  setSession = liftEvalM . setSession
