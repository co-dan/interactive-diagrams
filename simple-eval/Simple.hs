{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Monoid

import Data.Text.Lazy (pack)

import qualified Diagrams.Prelude as D
import Diagrams.Backend.SVG

import Mueval.Interpreter
import Mueval.Parallel
import qualified Mueval.ArgsParse as MArgs
import Mueval.Context
import qualified Language.Haskell.Interpreter as I
import Control.Concurrent.MVar (putMVar, MVar)
import Control.Concurrent (forkIO, myThreadId, ThreadId)

-- import Interp

diagramModules :: [String]
diagramModules = [ "Diagrams.Prelude"
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


type InterpResult = Either I.InterpreterError (String,String,String)

runInterp :: MArgs.Options -> MVar InterpResult -> IO ThreadId
runInterp opts mvar = do
  mainId <- myThreadId
  watchDog (MArgs.timeLimit opts) mainId
  forkIO $ do
    r <- liftIO (I.runInterpreter (interpreter opts))
    putMVar mvar r
  
serve :: ScottyM ()
serve = do
  middleware logStdoutDev

  get "/" $ do
    html $ mconcat [ "<form method=POST action=\"diagrams\">"
                   , "Input your code: <br />"
                   , "<textarea name=\"code\"></textarea>"
                   , "<br />"
                   , "<input type=submit value=\"Eval\">"
                   , "</form>"]
  
  post "/diagrams" $ do
    code <- param "code"
    let opts = options { MArgs.expression = code }
    r <- liftIO $ block runInterp opts
    case r of
      Left err -> text $ pack (show err)
      Right (e,et,val) -> text $ pack val

  
staticServe :: ScottyM ()
staticServe = do
  middleware $ staticPolicy (addBase "static")
  
main = scotty 3000 (staticServe >> serve)
