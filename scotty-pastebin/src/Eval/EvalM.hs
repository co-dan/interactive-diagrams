{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Eval.EvalM where

import Control.Monad.Trans
import Control.Monad.State (StateT(..), MonadState(..),
                            evalStateT, runStateT)
import Data.Default

import GHC
import qualified MonadUtils

  
import Eval.EvalSettings
  
newtype EvalM a = EvalM (StateT EvalSettings Ghc a)
  deriving (Monad, MonadState EvalSettings,
            MonadIO)


liftEvalM :: Ghc a -> EvalM a
liftEvalM = EvalM . lift

evalEvalM :: EvalM a -> Ghc a
evalEvalM (EvalM act') = evalStateT act' def

runEvalM :: EvalM a -> Ghc (a, EvalSettings)
runEvalM (EvalM act') = runStateT act' def

instance MonadIO Ghc where
  liftIO = MonadUtils.liftIO
