{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, RecordWildCards #-}
module Main where

import Control.Monad (forM_, when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either (EitherT(..), eitherT)
import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Control.Monad.Trans.Maybe (MaybeT(..))

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
import Util (runWithSql, getDR, intToKey,
             keyToInt, hash, getPastesDir)
import Eval
  
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
    H.title $ "Evaluate -> " <> (H.toHtml title)
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/style.css"
  H.body $ do
    H.h1 (H.toHtml title)
    H.div ! HA.id "main" $ content
    

formWithCode :: Text -> Html
formWithCode code = do
  H.div ! class_ "input" $
    H.div ! HA.id "form" $
      H.form ! action "/new" ! method "POST" $ do
        H.p "Input your code:"
        H.br
        H.textarea ! rows "20" ! cols "80" ! name "code" $ H.toHtml code
        H.br
        H.input ! type_ "Submit" ! value "Eval"


renderPaste :: Paste -> ActionM ()
renderPaste Paste{..} = html . renderHtml . mainPage "Paste" $ do
  formWithCode pasteContent
  H.div ! class_ "output" $
    H.div ! HA.id "sheet" $ do
      foldMap (H.preEscapedToHtml . Display.result) (getDR pasteResult)

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
    H.hr
    forM_ pastes $ \(Entity k' Paste{..}) -> do
      let k = keyToInt k'
      H.a ! href (H.toValue ("/get/" ++ (show k))) $
        H.toHtml $ "Paste id " ++ (show k)
      H.br

errPage :: Text -> Text -> ActionM ()
errPage code msg = do
  html . renderHtml . mainPage "Error" $ do
    formWithCode code
    H.hr
    H.toHtml msg
  
newPaste :: EitherT (Text,Text) ActionM Int
newPaste = do
  code <- lift (param "code")
  when (T.null code) $ throwT (code,"Empty input")
  pid <- compilePaste code
         `catchT` \e -> throwT (code, e)
  return (keyToInt pid)

compilePaste :: Text -> EitherT Text ActionM (Key Paste)
compilePaste code = do
  fname <- liftIO $ hash code
  let fpath = getPastesDir </> show fname ++ ".hs"
  liftIO $ T.writeFile fpath code
  res <- liftIO $ loadFile fpath
  case res of
    Left err -> throwT (pack err)
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
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "../common/static")
    S.get "/get/:id" $ maybeT page404 renderPaste getPaste
    S.get "/" listPastes
    S.post "/new" $ eitherT (uncurry errPage) redirPaste (measureTime newPaste)
