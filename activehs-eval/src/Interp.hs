module Interp where

import ActiveHS.Simple as AHS
import Language.Haskell.Interpreter as I
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Concurrent   (forkIO, killThread, myThreadId, threadDelay, throwTo, ThreadId)
import Control.Monad
import Data.Typeable as T

import Display
import SignalHandlers

startInterp :: IO TaskChan
startInterp = liftM fst $ startGHCiServer ["."] putStrLn putStrLn
              
setupInterp :: String -> Interpreter DisplayResult
setupInterp expr = do
  set [ languageExtensions := [ExtendedDefaultRules,NoMonomorphismRestriction] ]
  let ms = defaultModules ++ diagramModules
  let unqualModules =  zip ms (repeat Nothing)
  setImportsQ (unqualModules)
  I.interpret ("display (" ++ expr ++ ")") infer

runInterp :: String -> TaskChan -> IO (IError DisplayResult)
runInterp code tc = do
  r <- AHS.interpret tc "Helpers" $ setupInterp code
  restoreHandlers
  return r

diagramModules :: [String]
diagramModules = [ "Diagrams.Prelude"
                 , "Diagrams.Backend.SVG"
                 , "Diagrams.Core" ]

defaultModules :: [String]
defaultModules = [ "Prelude"
                 , "Display"]
