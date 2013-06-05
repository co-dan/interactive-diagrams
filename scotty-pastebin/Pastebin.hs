{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, RecordWildCards #-}
import Web.Scotty as S

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.HTTP.Types

import Control.Monad (forM_)
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
import Util (runWithSql, getDR, intToKey, keyToInt)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Paste
    content String
    result DisplayResult
    deriving Show
|]

-- | * Rendering and views

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

-- | * Database access and logic

getPaste :: MaybeT ActionM Paste
getPaste = do 
  pid <- hoistMaybe . readMaybe =<< lift (param "id")
  paste <- liftIO $ runWithSql $ P.get (intToKey pid)
  hoistMaybe paste

-- | ** Selects 20 recent pastes
listPastes :: ActionM ()
listPastes = do
  pastes <- liftIO $ runWithSql $ 
    selectList [] [LimitTo 20]
  html . renderHtml . mainPage "Paste" $ do
    formWithCode ""
    H.hr
    forM_ pastes $ \(Entity k' Paste{..}) -> do
      let k = keyToInt k'
      H.a ! href (H.toValue ("/get/" ++ (show k))) $ H.toHtml (show k)
      H.br

page404 :: ActionM ()
page404 = do
  status status404
  text "Not found"
  
main :: IO ()
main = do
  runWithSql (runMigration migrateAll)
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "../common/static")
    S.get "/get/:id" $ maybeT page404 renderPaste getPaste
    S.get "/" listPastes
