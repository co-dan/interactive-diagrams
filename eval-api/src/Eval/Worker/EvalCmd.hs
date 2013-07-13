{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}
module Eval.Worker.EvalCmd where

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Serialize (Serialize)
import Data.Typeable
import GHC.Generics
import System.FilePath.Posix ((</>))

import InteractiveEval hiding (compileExpr)

import Display
import Eval (traceM)
import Eval.EvalM
import Eval.EvalSettings
import Eval.Helpers  
import Eval.Worker.Types
  
data EvalCmd = CompileFile FilePath
               -- ^ Path to the file
             | EvalString  String
               -- ^ Expression to evaluate
             | EvalFile    String    Text
               -- ^  Name of the file, contents
             deriving (Typeable, Generic)

instance Serialize EvalCmd

evalCmdToEvalM :: EvalCmd -> EvalM DisplayResult
evalCmdToEvalM (CompileFile fpath) = do
  loadFile fpath
  underIO <- isUnderIO "main"
  if underIO
    then compileExpr "(return . display =<< main) :: IO DisplayResult"
    else compileExpr "(display (main)) :: DisplayResult"
    
evalCmdToEvalM (EvalString s) = compileExpr s
evalCmdToEvalM (EvalFile n txt) = do
  EvalSettings{..} <- ask
  let fpath = tmpDirPath </> n
--  traceM fpath
  liftIO $ T.writeFile fpath txt
  evalCmdToEvalM (CompileFile fpath)

data WStatus = OK | Timeout | Unknown
             deriving (Generic, Show)

instance Serialize WStatus
         
data ServiceCmd = RequestWorker
                | RequestWorkerMaybe
                | ReturnWorker WStatus (Worker EvalWorker)
                deriving (Generic)

instance Serialize ServiceCmd                         
