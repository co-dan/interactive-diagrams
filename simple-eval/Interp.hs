module Interp where

import Mueval.Interpreter
import Mueval.Parallel
import qualified Mueval.ArgsParse as MArgs
import Mueval.Context
import Language.Haskell.Interpreter as I
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Concurrent   (forkIO, killThread, myThreadId, threadDelay, throwTo, ThreadId)
import Control.Monad
import Data.Typeable as T

import Display

setInterp :: MArgs.Options -> Interpreter DisplayResult
setInterp opts = do
  set [ languageExtensions := (ExtendedDefaultRules:NoMonomorphismRestriction:glasgowExtensions) ]
  loadModules ["*Display.hs"]
  setTopLevelModules ["Display"]
  case MArgs.modules opts of
    Nothing -> return ()
    Just ms -> do let unqualModules =  zip ms (repeat Nothing)
                  setImportsQ (unqualModules)
  let expr = MArgs.expression opts
  res <- interpret ("display (" ++ expr ++ ")") infer
  return res

diagramModules :: [String]
diagramModules = [ "Diagrams.Prelude"
                 , "Diagrams.Backend.SVG"
                 , "Diagrams.Core" ]

options :: MArgs.Options
options = MArgs.Options { MArgs.expression = ""
                        , MArgs.modules =
                          Just $ defaultModules ++ diagramModules
                        , MArgs.timeLimit = 1
                        , MArgs.user = ""
                        , MArgs.loadFile = ""
                        , MArgs.printType = False
                        , MArgs.extensions = False
                        , MArgs.namedExtensions = []
                        , MArgs.noImports = False
                        , MArgs.rLimits = False
                        , MArgs.packageTrust = False
                        , MArgs.trustedPackages = defaultPackages
                        , MArgs.help = False }
