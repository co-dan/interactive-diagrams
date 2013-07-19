{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either (EitherT(..), eitherT)
import Data.Monoid ((<>), mempty)
import Data.Foldable (foldMap)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Network (listenOn, connectTo, accept, socketPort, PortID(..), Socket(..))
import System.IO (hClose, Handle)

import Data.Typeable
import Data.Data
import Data.Aeson ()
import Data.Default
import Data.Maybe (isJust, fromJust)
import Data.EitherR (throwT, catchT)
import Control.Error.Util (hoistMaybe, maybeT)
import System.FilePath.Posix ((</>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Web.Scotty as S
import Web.Scotty.Types
import Web.Scotty.Hastache
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

  
import Display (display, result)
import Paste    
import Util (controlSock, runWithSql, getDR, intToKey,
             keyToInt, hash, getPastesDir, renderDR, hasImage)
import Eval
import Eval.EvalError  
import Eval.EvalSettings
import Eval.EvalM
import Eval.Helpers
import Eval.Worker.EvalCmd
import Eval.Worker.Protocol
import Eval.Worker.Types
import Eval.Worker.Internal


hastacheConf :: MonadIO m => MuConfig m
hastacheConf = defaultConfig
   { muTemplateFileDir = Just "../common/templates/"
   , muTemplateFileExt = Just ".html"
   , muEscapeFunc      = emptyEscape
   }
   
-- | * Rendering and views

data ErrMsg = ErrMsg { content :: Text, caption :: String, severity :: String }
            deriving (Typeable, Data)

mkErrMsg :: EvalError -> ErrMsg
mkErrMsg EvalError{..} = ErrMsg
   { content  = renderHtml $
                foldMap ((<> H.br) . H.toHtml . T.pack) (lines errMsg)
   , caption  = caption
   , severity = style }
  where (style, caption) = case severity of
          SevError -> ("alert-error", "Error")
          SevWarning -> ("alert-block", "Warning")
          SevFatal -> ("alert-error", "Error")
          _ -> ("alert-info", "Info")

errPage :: Text -> (Text, [EvalError]) -> ActionH ()
errPage code (msg, errors) = do
  let errmsgs = map (mkGenericContext . mkErrMsg) errors
  setH "title"  $ MuVariable ("Error :(" :: Text)     
  setH "msg"    $ MuVariable msg 
  setH "errors" $ MuList errmsgs                      
  hastache "main"


renderPaste :: Paste -> ActionH ()
renderPaste Paste{..} = do
  setH "code"   $ MuVariable pasteContent
  setH "title"  $ MuVariable ("Paste" :: Text)
  setH "result" $ MuVariable $ renderHtml $
    foldMap renderDR (getDR pasteResult)
  hastache "main"


renderPasteList :: [Entity Paste] -> ActionH ()
renderPasteList pastes = do
  setH "result" $ MuBool False
  setH "title"  $ MuVariable ("Paste" :: Text)
  setH "pastes" $ MuList $
                      map (\(Entity k _) ->
                            (mkStrContext $ \("k") ->
                              MuVariable . show . keyToInt $ k))
                      pastes
  hastache "main"


data GalleryItem = GalleryItem { k :: Int, image :: T.Text }
                 deriving (Data, Typeable)

mkItem (k, p) = GalleryItem { k = k
                            , image = fromJust . hasImage
                                      . pasteResult $ p }
                
renderGallery :: [(Int, Paste)] -> ActionH ()
renderGallery ps = do
  let pastes = map (mkGenericContext . mkItem) ps
  setH "title"  $ MuVariable ("Gallery" :: T.Text)
  setH "images" $ MuList pastes
  hastache "gallery"
  
                   
-- | * Database access and logic

getPaste :: MaybeT (ActionT HState) Paste
getPaste = do 
  -- pid <- hoistMaybe . readMaybe =<< lift (param "id")
  pid <- lift $ param "id"
  paste <- liftIO $ runWithSql $ P.get (intToKey pid)
  hoistMaybe paste

-- | ** Select 20 recent images
listImages :: ActionH [(Int, Paste)]
listImages = do
  pastes <- liftIO $ runWithSql $
    rawSql "SELECT ?? FROM \"Paste\" WHERE (\"containsImg\"=?) ORDER BY RANDOM() LIMIT 20" [toPersistValue True]
  --  selectList [PasteContainsImg ==. True] [LimitTo 20, Desc PasteId]
  return (map getP pastes)
  where getP (Entity k p) = (keyToInt k, p)
        
-- | ** Select 20 recent pastes
listPastes :: ActionH ()
listPastes = do
  pastes <- liftIO $ runWithSql $ 
    selectList [] [LimitTo 20, Desc PasteId]
  renderPasteList pastes


-- newPaste :: EitherT (Text, (Text, [EvalError])) ActionM Int
newPaste = do
  code <- lift (param "code")
  when (T.null code) $ throwT (code, ("Empty input", []))
  pid <- compilePaste code
         `catchT` \e -> throwT (code, e)
  return (keyToInt pid)

-- compilePaste :: Text
--              -> EitherT (Text, [EvalError]) ActionM (Key Paste)
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
    Right r -> do
      let dr = display r
      let containsImage = isJust (hasImage dr)
      liftIO . runWithSql $ insert $
               Paste code (display r) containsImage
  
redirPaste :: Monad m => Int -> ActionT m ()
redirPaste i = redirect $ pack ("/get/" ++ show i)

page404 :: Monad m => ActionT m ()
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
  scottyH 3000 $ do
    -- setTemplatesDir "../common/templates/"
    setHastacheConfig hastacheConf
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "../common/static")
    S.get "/get/:id" $ maybeT page404 renderPaste getPaste
    S.get "/json/:id" $ maybeT page404 json getPaste
    S.get "/" listPastes
    S.get "/gallery" (listImages >>= renderGallery)
    S.post "/new" $ eitherT (uncurry errPage) redirPaste (measureTime newPaste)
