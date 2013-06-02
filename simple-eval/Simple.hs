{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Monoid

import Data.Text.Lazy (pack, Text(..))
import Text.Blaze.Html5 ((!), Html)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text

import qualified Diagrams.Prelude as D
import Diagrams.Backend.SVG

import Mueval.Interpreter
import Mueval.Parallel hiding (watchDog)
import qualified Mueval.ArgsParse as MArgs
import Mueval.Context
import qualified Language.Haskell.Interpreter as I
import Control.Concurrent.MVar (putMVar, MVar)
import Control.Concurrent (forkIO, myThreadId, ThreadId, killThread, threadDelay, throwTo)

import Display (DisplayResult(..), DR(..))
import Interp

type InterpResult = Either I.InterpreterError DisplayResult

formWithCode :: Text -> Html
formWithCode code = do
  H.form ! action "/diagrams" ! method "POST" $ do
    H.p "Input your code:"
    H.br
    H.textarea ! rows "20" ! cols "80" ! name "code" $ H.toHtml code
    H.br
    H.input ! type_ "Submit" ! value "Eval"


runInterp :: MArgs.Options -> MVar InterpResult -> IO ThreadId
runInterp opts mvar = do
  mainId <- myThreadId
  forkIO $ do
    r <- liftIO (I.runInterpreter (setInterp opts))
    putMVar mvar r
  
serve :: ScottyM ()
serve = do
  middleware logStdoutDev

  get "/" $ html . renderHtml . H.docTypeHtml $ do
    H.head $ H.title "Evaluate"
    H.body $ formWithCode ""
  
  post "/diagrams" $ do
    code <- param "code"
    let opts = options { MArgs.expression = code }
    r <- liftIO $ block runInterp opts
    case r of
      Left err -> text $ pack (show err)
      Right (DisplayResult drs) -> html . renderHtml . H.docTypeHtml $ do
        H.head $ H.title "Evaluate"
        H.body $ do
          formWithCode (pack code)
          H.br
          H.div ! style "background-color:grey; padding:12px;" $ do
            mconcat $ map (H.preEscapedToHtml . result) drs
      
  
staticServe :: ScottyM ()
staticServe = do
  middleware $ staticPolicy (addBase "static")
  
main = scotty 3000 (staticServe >> serve)
