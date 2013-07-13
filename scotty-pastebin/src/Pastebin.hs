{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (forM_, when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either (EitherT(..), eitherT)
import Data.Monoid ((<>), mempty)
import Data.Foldable (foldMap)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Network (listenOn, connectTo, accept, socketPort, PortID(..), Socket(..))
import System.IO (hClose, Handle)

import Data.Default
import Data.EitherR (throwT, catchT)
import Control.Error.Util (hoistMaybe, maybeT)
import System.FilePath.Posix ((</>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

  
import Web.Scotty as S
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.HTTP.Types

import Text.Blaze.Html5 ((!), Html)
import Text.Blaze.Html5.Attributes (type_, class_, href, rel, action, method,
                                    name, value, cols, rows)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text

import Database.Persist as P
import Database.Persist.TH as P
import Database.Persist.Sqlite as P

  
import Display hiding (text,html)
import DisplayPersist
import Util (controlSock, runWithSql, getDR, intToKey,
             keyToInt, hash, getPastesDir, renderDR)
import Eval
import Eval.EvalError  
import Eval.EvalSettings
import Eval.EvalM
import Eval.Helpers
import Eval.Worker.EvalCmd
import Eval.Worker.Protocol
import Eval.Worker.Types
import Eval.Worker.Internal

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Paste
    content Text
    result DisplayResult
    deriving Show
|]

-- | * Rendering and views

mainPage :: String -> Html -> Html
mainPage title content = H.docTypeHtml $ do
  H.head $ do
    H.title $ "Evaluate -> " <> H.toHtml title
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap.min.css"
    H.script ! HA.src "/js/bootstrap.min.js" $ mempty
    H.script ! HA.src "http://code.jquery.com/jquery-2.0.2.min.js" $ mempty
  H.body $ 
    H.div ! class_ "container-fluid" $ do
      H.div ! class_ "row-fluid" $
        H.div ! class_ "span12" $
          H.h1 (H.toHtml title)
      H.div ! class_ "row-fluid" 
        $ content
      H.div ! class_ "row-fluid" $
        H.a ! HA.href "https://github.com/co-dan/interactive-diagrams/releases/tag/first-demo" 
 	  $ H.toHtml ("More info" :: Text)
    

formWithCode :: Text -> Html
formWithCode code = 
  H.div ! class_ "span6" 
        ! HA.style "padding-right:20px; border-right: 1px solid #ccc;" $
    H.div ! HA.id "form" $
      H.form ! action "/new" ! method "POST" $ do
        H.p "Input your code:"
        H.textarea ! rows "20" ! name "code" ! class_ "row-fluid" $ H.toHtml code
        H.input ! type_ "Submit" ! class_ "btn" ! value "Eval"


renderPaste :: Int -> Paste -> ActionM ()
renderPaste sz Paste{..} = html . renderHtml . mainPage "Paste" $ do
  formWithCode pasteContent
  H.div ! class_ "span5" $
    H.div ! HA.id "sheet" $ 
--      H.pre $
        foldMap (renderDR sz) (getDR pasteResult)


-- | * Database access and logic


getPaste :: MaybeT ActionM Paste
getPaste = do 
  -- pid <- hoistMaybe . readMaybe =<< lift (param "id")
  pid <- lift $ param "id"
  paste <- liftIO $ runWithSql $ P.get (intToKey pid)
  hoistMaybe paste


-- | ** Selects 20 recent pastes
listPastes :: ActionM ()
listPastes = do
  pastes <- liftIO $ runWithSql $ 
    selectList [] [LimitTo 20, Desc PasteId]
  html . renderHtml . mainPage "Paste" $ do
    formWithCode ""
    H.div ! class_ "span5" $ 
      forM_ pastes $ \(Entity k' Paste{..}) -> do
        let k = keyToInt k'
        H.a ! href (H.toValue ("/get/" ++ show k)) $
          H.toHtml $ "Paste id " ++ show k
        H.br


errPage :: Text -> (Text, [EvalError]) -> ActionM ()
errPage code (msg, errors) = 
  html . renderHtml . mainPage "Error" $ do
    formWithCode code
    H.div ! class_ "span5" $ do
      H.div ! HA.id "sheet" $ 
        H.p $ H.toHtml msg
      forM_ errors $ \EvalError{..} -> 
        H.div ! HA.id "error" $ do
          let (style, caption) = case severity of
                SevError -> ("alert-error", "Error")
                SevWarning -> ("alert-block", "Warning")
                SevFatal -> ("alert-error", "Error")
                _ -> ("alert-info", "Info")
          H.div ! class_ ("alert " <> style) $ do
            H.button ! type_ "button" ! class_ "close"
                     ! H.dataAttribute "dissmis" "alert" $
                         H.preEscapedToHtml ("&times;" :: String)
            H.strong caption
            H.br
            foldMap ((<> H.br) . H.toHtml . T.pack) (lines errMsg)
        
  
newPaste :: EitherT (Text, (Text, [EvalError])) ActionM Int
newPaste = do
  code <- lift (param "code")
  when (T.null code) $ throwT (code, ("Empty input", []))
  pid <- compilePaste code
         `catchT` \e -> throwT (code, e)
  return (keyToInt pid)

compilePaste :: Text
             -> EitherT (Text, [EvalError]) ActionM (Key Paste)
compilePaste code = do
  fname <- liftIO $ hash code
  -- let fpath = getPastesDir </> show fname ++ ".hs"
  -- liftIO $ T.writeFile fpath code
  hndl <- liftIO $ connectTo "localhost" (UnixSocket controlSock)
  liftIO $ sendData hndl RequestWorker
  (worker :: Worker EvalWorker) <- liftIO $ getData hndl
  -- liftIO $ hClose hndl
  ((res, errors), status) <- liftIO $ sendEvalRequestNoRestart worker $
                             EvalFile (show fname ++ ".hs") code
                             -- CompileFile fpath
  hndl <- liftIO $ connectTo "localhost" (UnixSocket controlSock)
  liftIO $ sendData hndl (ReturnWorker status worker)
  -- liftIO $ hClose hndl
  case res of
    Left err -> throwT (pack err, errors)
    Right r -> liftIO . runWithSql $ insert $
               Paste code (display r)
  
redirPaste :: Int -> ActionM ()
redirPaste i = redirect $ pack ("/get/" ++ show i)

page404 :: ActionM ()
page404 = do
  status status404
  text "Not found"
  
measureTime :: MonadIO m => m a -> m a
measureTime act = do
  t0 <- liftIO getCurrentTime
  res <- act
  t1 <- liftIO getCurrentTime
  liftIO $ putStrLn $ "Time elapsed: " ++ show (diffUTCTime t1 t0)
  return res
  

main :: IO ()
main = do
  runWithSql (runMigration migrateAll)
  -- (queue, _) <- prepareEvalQueue (def {tmpDirPath = getPastesDir, rlimits = Just def})
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "../common/static")
    S.get "/get/:id" $ maybeT page404 (renderPaste 500) getPaste
    S.get "/" listPastes
    S.post "/new" $ eitherT (uncurry errPage) redirPaste (measureTime newPaste)

