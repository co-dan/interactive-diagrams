{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Eval.Worker.EvalCmd where

import Data.Serialize (Serialize)
import Data.Typeable
import GHC.Generics
  
import Display
import Eval.EvalM
import Eval.Helpers  
import Eval.Worker.Types
  
data EvalCmd = CompileFile FilePath
             | EvalString  String
             deriving (Typeable, Generic)

instance Serialize EvalCmd

evalCmdToEvalM :: EvalCmd -> EvalM DisplayResult
evalCmdToEvalM (CompileFile fpath) = do
  loadFile fpath
  compileExpr "return . display =<< main"
evalCmdToEvalM (EvalString s) = compileExpr s


data WStatus = OK | Timeout | Unknown
             deriving (Generic, Show)

instance Serialize WStatus
         
data ServiceCmd = RequestWorker
                | RequestWorkerMaybe
                | ReturnWorker WStatus (Worker EvalWorker)
                deriving (Generic)

instance Serialize ServiceCmd                         
