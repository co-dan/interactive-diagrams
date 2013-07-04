{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Eval.Worker.EvalCmd where

import Data.Serialize (Serialize)
import Data.Typeable
import GHC.Generics
  
import Eval.EvalM
import Eval.Helpers  
import Display

data EvalCmd = CompileFile FilePath
             | EvalString  String
             deriving (Typeable, Generic)

instance Serialize EvalCmd

evalCmdToEvalM :: EvalCmd -> EvalM DisplayResult
evalCmdToEvalM (CompileFile fpath) = do
  loadFile fpath
  compileExpr "return . display =<< main"
evalCmdToEvalM (EvalString s) = compileExpr s
