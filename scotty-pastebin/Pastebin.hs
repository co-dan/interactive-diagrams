{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, RecordWildCards #-}
import Web.Scotty as S

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Control.Monad.Trans
import Data.Monoid
import Data.Foldable (foldMap)

import Control.Monad.Trans.Maybe
import Control.Error.Util

import Text.Read (readMaybe)
import Data.Text.Lazy (pack, Text)
import Text.Blaze.Html5 ((!), Html)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text

import Database.Persist as P
import Database.Persist.TH as P
import Database.Persist.Sqlite as P

import Display hiding (text,html)
import DisplayPersist
import Util (runWithSql, getDR)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Paste
    content String
    result DisplayResult
    deriving Show
|]

-- | Rendering and views

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


renderPaste :: Paste -> ActionM ()
renderPaste Paste{..} = html . renderHtml . mainPage "Paste" $ do
  formWithCode (pack pasteContent)
  H.div ! class_ "output" $
    H.div ! HA.id "sheet" $ do
      foldMap (H.preEscapedToHtml . Display.result) (getDR pasteResult)

-- | Database access and logic

getPaste :: MaybeT ActionM Paste
getPaste = do 
  pid <- hoistMaybe . readMaybe =<< lift (param "id")
  paste <- liftIO $ runWithSql $ P.get (Key . PersistInt64 $ pid)
  hoistMaybe paste

main :: IO ()
main = do
  runWithSql (runMigration migrateAll)
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "../common/static")
    S.get "/get/:id" $ maybeT (raise "Invalid id") renderPaste getPaste

