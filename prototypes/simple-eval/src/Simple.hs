{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import Web.Scotty

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Control.Monad.Trans
import Data.Monoid

import Data.Text.Lazy (pack, Text(..))
import Text.Blaze.Html5 ((!), Html)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text

import Mueval.Parallel hiding (watchDog)
import qualified Mueval.ArgsParse as MArgs
import qualified Language.Haskell.Interpreter as I
import Control.Concurrent.MVar (putMVar, MVar)
import Control.Concurrent (forkIO, myThreadId, ThreadId, killThread, threadDelay, throwTo)

import Display (DisplayResult(..), DR(..))
import Interp
import SignalHandlers

type InterpResult = Either I.InterpreterError DisplayResult

mainPage :: String -> Html -> Html
mainPage title content = H.docTypeHtml $ do
  H.head $ do
    H.title $ "Evaluate -> " <> (H.toHtml title)
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/style.css"
  H.body $ do
    H.h1 (H.toHtml title)
    H.div ! HA.id "main" $ content
    

formWithCode :: Text -> Html
formWithCode code = do
  H.div ! class_ "input" $
    H.div ! HA.id "form" $
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

  get "/" $ html . renderHtml $ mainPage "main" (formWithCode "")
  
  post "/diagrams" $ do
    code <- param "code"
    let opts = options { MArgs.expression = code }
    r <- liftIO $ block runInterp opts
    liftIO $ restoreHandlers
    case r of
      Left err -> text $ pack (show err)
      Right (DisplayResult drs) -> html . renderHtml $ 
        mainPage "result" $ do
          formWithCode (pack code)
          H.div ! class_ "output" $
            H.div ! HA.id "sheet" $ do
              mconcat $ map (H.preEscapedToHtml . result) drs
      
  
staticServe :: ScottyM ()
staticServe = do
  middleware $ staticPolicy (addBase "../common/static")
  
main = scotty 3000 (staticServe >> serve)
