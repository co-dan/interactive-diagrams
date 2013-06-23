module Eval.Helpers where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Data.Serialize (encode, decode, Serialize)

import GHC
import MonadUtils hiding (MonadIO, liftIO)
import Outputable
import Exception

import Eval.EvalError
import Eval.EvalM 
import Display

-- | Compiles the 'main' action in the source code file
-- to a @DisplayResult@
compileFile :: FilePath -> EvalM DisplayResult
compileFile file = liftEvalM $ do
  setTargets =<< sequence [ guessTarget file Nothing
                          , guessTarget "Helper.hs" Nothing]
  graph <- depanal [] False
  -- output graph
  loaded <- load LoadAllTargets
  when (failed loaded) $ throw LoadingException
  setContext (map (IIModule . moduleName . ms_mod) graph)
  let expr = "return . display =<< main"
  ty <- exprType expr -- throws exception if doesn't typecheck
  -- output ty
  res <- unsafePerformIO . unsafeCoerce <$> compileExpr expr
  return res

-- | Outputs any value that can be pretty-printed using the default style
output :: Outputable a => a -> Ghc ()
output a = do
  dfs <- getSessionDynFlags
  let style = defaultUserStyle
      cntx = initSDocContext dfs style
  liftIO $ print $ runSDoc (ppr a) cntx
