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

-- import qualified Diagrams.Prelude as D
-- import Diagrams.Backend.SVG

import qualified Language.Haskell.Interpreter as I
import Control.Concurrent.MVar (putMVar, MVar)
import Control.Concurrent (forkIO, myThreadId, ThreadId, killThread, threadDelay, throwTo)

import Display (DisplayResult(..), DR(..))
import Interp
import ActiveHS.Simple

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

  
serve :: TaskChan -> ScottyM ()
serve tc = do
  middleware logStdoutDev

  get "/" $ html . renderHtml $ mainPage "main" (formWithCode "")
  
  post "/diagrams" $ do
    code <- param "code"
    r <- liftIO $ runInterp code tc
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

scottyTest :: TaskChan -> ScottyM ()
scottyTest tc = do
  middleware logStdoutDev

  get "/" $ do
    code <- param "code"
    r <- liftIO $ runInterp code tc
    case r of
      Left err -> text . pack $ show err
      Right (DisplayResult dr) -> text . pack $ show dr

  
main = do
   (tc,tid) <- startGHCiServer ["."] putStrLn putStrLn
   scotty 3000 $ staticServe >> serve tc
   killThread tid

-- interp :: String -> I.Interpreter DisplayResult
-- interp i = do
--   I.set [I.searchPath I.:= ["."]]
--   I.loadModules ["*Display.hs"]
--   I.setTopLevelModules ["Display"]
--   I.interpret ("display (" ++ i ++ ")") I.infer

-- main = do
--   code <- getLine
--   r <- I.runInterpreter (interp code)
--   case r of
--     Left err -> print $ show err
--     Right (DisplayResult dr) -> print dr
  
