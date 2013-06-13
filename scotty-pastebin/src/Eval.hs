{-# LANGUAGE ScopedTypeVariables #-}
module Eval where

import GHC
import GHC.Paths
import HscTypes
import Data.Dynamic
import MonadUtils
import Unsafe.Coerce
import System.IO.Unsafe

import Display

import Control.Applicative
import Control.Concurrent
import System.Posix.Process  

import Outputable

-- run :: Display a => Ghc a -> IO a
run m = run' (initGhc >> m)

-- run' :: Display a => Ghc a -> IO a
run' m = runGhc (Just libdir) m

output :: Outputable a => a -> Ghc ()
output a = do
  dfs <- getSessionDynFlags
  let style = defaultUserStyle
      cntx = initSDocContext dfs style
  liftIO $ print $ runSDoc (ppr a) cntx

initGhc :: Ghc ()
initGhc = do
  dfs <- getSessionDynFlags
  setSessionDynFlags $ dfs { hscTarget = HscInterpreted
                           , ghcLink = LinkInMemory }
  return ()

compileFile :: FilePath -> Ghc DisplayResult
compileFile file = do
  setTargets =<< sequence [ guessTarget file Nothing
                          , guessTarget "Helper.hs" Nothing]
  -- output target
  graph <- depanal [] False
  load LoadAllTargets
  setContext (map (IIModule . moduleName . ms_mod) graph)
  let expr = "return . display =<< main"
  ty <- exprType expr -- throws exception if doesn't typecheck
  -- output ty
  res <- unsafePerformIO . unsafeCoerce <$> compileExpr expr
  return res

test :: IO DisplayResult
test = run (compileFile "./test/file1.hs")
