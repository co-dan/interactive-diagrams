{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeHoles                  #-}
module Main where

import           Control.Applicative                  ((<$>))
import           Control.Monad                        (when)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Trans                  (lift)
import           Control.Monad.Trans.Either           (EitherT (..), eitherT)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Foldable                        (foldMap)
import           Data.Monoid                          (mconcat)
import           Network                              (PortID (..), connectTo)
import           System.IO                            (hClose)

import           Control.Error.Util                   (hoistMaybe, maybeT)
import           Data.Aeson                           ()
import           Data.EitherR                         (catchT, throwT)
import           Data.Maybe                           (isJust)
import           Data.Text.Lazy                       (Text, pack)
import qualified Data.Text.Lazy                       as T
import           Data.Time.Clock                      (diffUTCTime,
                                                       getCurrentTime)

import           Network.HTTP.Types
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Text.Hastache
import           Text.Hastache.Context
import           Web.Scotty                           as S
import           Web.Scotty.Hastache
import           Web.Scotty.Types

import           Database.Persist                     as P
import           Database.Persist.Sqlite              as P
import           Text.Blaze.Html.Renderer.Text

import           Config
import           Diagrams.Interactive.Display         (display)
import           Eval
import           Eval.EvalError
import           Pastebin.ErrorMessage
import           Pastebin.Gallery
import           Pastebin.Paste
import           Pastebin.Util

hastacheConf :: MonadIO m => MuConfig m
hastacheConf = defaultConfig
   { muTemplateFileDir = Just getTemplatesDir
   , muTemplateFileExt = Just ".html"
   , muEscapeFunc      = emptyEscape
   }

-- | * Rendering and views

errPage :: (String, Text, Text, (Text, [EvalError])) -> ActionH ()
errPage (ptitle, author, code, (msg, errors)) = do
    let errmsgs = map (mkGenericContext . mkErrMsg) errors
    setH "title"  $ MuVariable ("Error :(" :: Text)
    setH "msg"    $ MuVariable msg
    setH "errors" $ MuList errmsgs
    setH "ptitle" $ MuVariable ptitle
    setH "author" $ MuVariable author
    setH "code"   $ MuVariable code
    hastache "main"


renderPaste :: Paste -> ActionH ()
renderPaste Paste{..} = do
    setH "code"     $ MuVariable pasteContent
    setH "codeView" $ MuVariable (renderCode pasteContent)
    setH "title"    $ MuVariable $ mconcat ["Paste / ", T.pack pasteTitle,
                                          " by ", pasteAuthor]
    setH "author"   $ MuVariable pasteAuthor
    setH "ptitle"   $ MuVariable pasteTitle
    setH "result"   $  MuVariable $ renderHtml $
        foldMap renderDR (getDR pasteResult)
    hastache "main"


renderPasteList :: [Entity Paste] -> ActionH ()
renderPasteList pastes = do
    setH "result" $ MuBool False
    setH "title"  $ MuVariable ("Paste" :: Text)
    setH "pastes" $ MuList $
                      map (\(Entity k pst) ->
                            (mkStrContext $ \cnt ->
                              case cnt of
                                "k"       -> MuVariable . show . keyToInt $ k
                                "ptitle"  -> MuVariable $ pasteTitle pst
                                "pauthor" -> MuVariable $ pasteAuthor pst
                                _         -> MuNothing
                            ))
                      pastes
    hastache "main"


renderGallery :: [(Int, Paste)] -> ActionH ()
renderGallery ps = do
    let pastes = map (mkGenericContext . mkItem) ps
    setH "title"  $ MuVariable ("Gallery" :: T.Text)
    setH "images" $ MuList pastes
    hastache "gallery"


-- | * Database access and logic

getPaste :: MaybeT (ActionT HState) Paste
getPaste = do
    pid <- lift $ param "id"
    paste <- liftIO $ runWithSql $ P.get (intToKey pid)
    hoistMaybe paste

-- | ** Select 20 recent images
listImages :: ActionH [(Int, Paste)]
listImages = do
    pastes <- liftIO $ runWithSql gallerySql
    return (map getP pastes)
  where
    getP (Entity k p) = (keyToInt k, p)

-- | ** Select 20 recent pastes
listPastes :: ActionH ()
listPastes = do
    pastes <- liftIO $ runWithSql $
              selectList [] [LimitTo 20, Desc PasteId]
    renderPasteList pastes


newPaste :: EitherT (String, Text, Text, (Text, [EvalError])) ActionH Int
newPaste = do
    title' <- T.unpack <$> lift (paramEscaped "title")
    let title = if (null title') then "(undefined)" else title'
    code <- lift (param "code")
    usern' <- lift (paramEscaped "author")
    let author = if (T.null usern') then "Anonymous" else usern'
    when (T.null code) $ throwT (title, author, code, ("Empty input", []))
    pid <- compilePaste title code author
           `catchT` \e -> throwT (title, author, code, e)
    return (keyToInt pid)

compilePaste :: MonadIO m
             => String -> Text -> Text
             -> EitherT (Text, [EvalError]) m (Key (PasteGeneric SqlBackend))
compilePaste title code author = do
    fname <- liftIO $ hash code
    -- let fpath = getPastesDir </> show fname ++ ".hs"
    -- liftIO $ T.writeFile fpath code
    hndl <- liftIO $ connectTo "localhost" (UnixSocket controlSock)
    _ <- liftIO $ sendData hndl RequestWorker
    (worker :: Worker EvalWorker) <- liftIO $ getData hndl
    liftIO $ hClose hndl
    ((res, errors), wstatus) <- liftIO $ sendEvalRequestNoRestart worker $
                                EvalFile (show fname ++ ".hs") code
                             -- CompileFile fpath
    hndl2 <- liftIO $ connectTo "localhost" (UnixSocket controlSock)
    _ <- liftIO $ sendData hndl2 (ReturnWorker wstatus worker)
    liftIO $ hClose hndl2
    case res of
        Left err -> throwT (pack err, errors)
        Right !r -> do
            let dr = display r
            let containsImage = isJust (hasImage dr)
            liftIO . runWithSql . insert $
                Paste title code (display r) containsImage author

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
        S.post "/new" $ eitherT errPage redirPaste (measureTime newPaste)
