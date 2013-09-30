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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeHoles                  #-}
module Main where

import           Control.Applicative                  ((<$>))
import           Control.Monad                        (when)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.State
import           Control.Monad.Trans                  (lift)
import           Control.Monad.Trans.Either           (EitherT (..), eitherT)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Control.Monad.Trans.Resource         (ResourceT,
                                                       transResourceT)
import           Data.Foldable                        (foldMap)
import           Data.Monoid                          (mconcat, mempty)
import           Network                              (PortID (..), connectTo)
import           System.IO                            (hClose)

import           Control.Error.Util                   (hoistMaybe, maybeT,
                                                       nothing)
import           Data.Aeson                           ()
import           Data.EitherR                         (catchT, throwT)
import           Data.Maybe                           (isJust)
import           Data.Text                            (Text, pack)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Data.Time.Clock                      (diffUTCTime,
                                                       getCurrentTime)
import           Data.Time.Format                     (formatTime)
import           System.Locale                        (defaultTimeLocale)


import           Database.Persist                     as P
import           Database.Persist.Sql                 as P
import           Database.Persist.Postgresql          as P
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.Gzip
import           Text.Blaze.Html.Renderer.Text
import           Text.Hastache
import           Text.Hastache.Context
import           Web.Scotty.Hastache
import           Web.Scotty.Trans                     as S

import           Config
import           Diagrams.Interactive.Display         (DisplayResult (..),
                                                       DynamicResult (..),
                                                       StaticResult (..),
                                                       display, result)
import           Diagrams.Interactive.Eval
import           Diagrams.Interactive.Eval.EvalError
import           Pastebin.ErrorMessage
import           Pastebin.Feed
import           Pastebin.Gallery
import           Pastebin.Paste
import           Pastebin.Util

hastacheConf :: MonadIO m => MuConfig m
hastacheConf = defaultConfig
   { muTemplateFileDir = Just getTemplatesDir
   , muTemplateFileExt = Just ".html"
   , muEscapeFunc      = emptyEscape -- dont escape
   }

-- | * Rendering and views

errPage :: (Paste, (Text, [EvalError])) -> ActionH ()
errPage (Paste{..}, (msg, errors)) = do
    let errmsgs = map (mkGenericContext . mkErrMsg) errors
    setH "title"    $ MuVariable ("Error :(" :: Text)
    setH "msg"      $ MuVariable msg
    setH "errors"   $ MuList errmsgs
    setH "ptitle"   $ MuVariable pasteTitle
    setH "author"   $ MuVariable pasteAuthor
    setH "literate" $ MuVariable pasteLiterateHs
    setH "code"     $ MuVariable pasteContent
    setH "current"  $ MuVariable $ fmap keyToInt pasteParent
    setH "date"     $ MuVariable (formatTime defaultTimeLocale "%c" pasteCreatedAt)
    hastache "main"


renderPaste :: (Entity Paste) -> ActionH ()
renderPaste (Entity k p@Paste{..}) = do
    setH "code"     $ MuVariable pasteContent
    setH "codeView" $ MuVariable (renderCode p)
    setH "title"    $ MuVariable $ mconcat ["Paste / ", T.pack pasteTitle,
                                          " by ", pasteAuthor]
    setH "author"   $ MuVariable pasteAuthor
    setH "date"     $ MuVariable (formatTime defaultTimeLocale "%c" pasteCreatedAt)
    setH "ptitle"   $ MuVariable pasteTitle
    setH "literate" $ MuVariable pasteLiterateHs
    setH "parent"   $ MuVariable $ fmap keyToInt pasteParent
    setH "current"  $ MuVariable $ keyToInt k
    setH "result"   $ MuVariable $ case pasteResult of
        Static staticRes ->
            renderHtml $ foldMap renderDR (getDR staticRes)
        Interactive dynRes   ->
            renderHtml $ renderJS dynRes (keyToInt k)
    hastache "main"


renderPasteList :: [(Single Int, Single String, Single Text)] -> ActionH ()
renderPasteList pastes = do
    setH "result" $ MuBool False
    setH "title"  $ MuVariable ("Paste" :: Text)
    setH "pastes" $ MuList $
                      map (\(k, ptitle, pauthor) ->
                            (mkStrContext $ \cnt ->
                              case cnt of
                                "k"       -> MuVariable . show $ unSingle k
                                "ptitle"  -> MuVariable $ unSingle ptitle
                                "pauthor" -> MuVariable $ unSingle pauthor
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


about :: ActionH ()
about = do
    setH "title" $ MuVariable ("Usage" :: T.Text)
    setH "about" $ MuVariable True
    hastache "usage"

-- | * Database access and logic

getRaw :: MaybeT ActionH Text
getRaw = do
    -- pid <- lift $ param "id"
    ind <- lift $ param "ind"
    (Entity _ paste) <- getPaste
    case pasteResult paste of
        Static (StaticResult res) ->
            return . result $ res !! ind
        Interactive (DynamicResult res) -> return res

getPaste :: MaybeT (ActionT HState) (Entity Paste)
getPaste = do
    pid <- lift $ param "id"
    paste <- liftIO $ runWithSql $ P.get (intToKey pid)
    hoistMaybe $ liftM (Entity (intToKey pid)) paste

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
              rawSql "SELECT id, title, author FROM \"Paste\" ORDER BY id DESC LIMIT 20" []
              --selectList [] [LimitTo 20, Desc PasteId]
    -- let pastes = []
    renderPasteList pastes

feed :: ActionH ()
feed = do
    pastes <- liftIO $ runWithSql $
              rawSql "SELECT id, title, content, \"literateHs\", author, \"createdAt\" FROM \"Paste\" ORDER BY id DESC LIMIT 20" []
              -- selectList [] [LimitTo 20, Desc PasteId]

    let pastes' = map (\(Single k, Single title, Single content, Single lhs, Single author, Single createdAt) -> (keyToInt k, Paste title content (Static $ StaticResult []) False lhs author createdAt Nothing)) pastes

    renderRss $ mkRssFeed pastes'

newPaste :: EitherT (Paste, (Text, [EvalError])) ActionH Int
newPaste = do
    title' <- T.unpack <$> lift (paramEscaped "title")
    let title = if (null title') then "(undefined)" else title'

    code <- lift (param "code")

    usern' <- lift (paramEscaped "author")
    let author = if (T.null usern') then "Anonymous" else usern'

    lhs <- lift (param "literate"
                 `rescue` (return (return False)))

    parentP' <- lift (paramMaybe "parent")

    let parentP = fmap intToKey parentP'
    -- parentP' <- lift (param "parent")
    -- let parentP = Just (intToKey parentP')
    now <- liftIO getCurrentTime

    let p = Paste title code (Static $ StaticResult []) False lhs author now parentP
    when (T.null code) $ throwT (p, ("Empty input", []))
    pid <- compilePaste p
           `catchT` \e -> throwT (p, e)

    return (keyToInt pid)

compilePaste :: MonadIO m
             => Paste
             -> EitherT (Text, [EvalError]) m (Key (PasteGeneric SqlBackend))
compilePaste (p@Paste{..}) = do
    fname <- liftIO $ hash pasteContent
    let extn = if pasteLiterateHs then ".lhs" else ".hs"
    hndl <- liftIO $ connectTo "localhost" (UnixSocket controlSock)
    _ <- liftIO $ sendData hndl RequestWorker
    (worker :: Worker EvalWorker) <- liftIO $ getData hndl
    liftIO $ hClose hndl
    ((res, errors), wstatus) <- liftIO $ sendEvalRequestNoRestart worker $
                                EvalFile (show fname ++ extn) pasteContent
    hndl2 <- liftIO $ connectTo "localhost" (UnixSocket controlSock)
    _ <- liftIO $ sendData hndl2 (ReturnWorker wstatus worker)
    liftIO $ hClose hndl2
    case res of
        Left err -> throwT (pack err, errors)
        Right (Static dr) -> do
            let containsImage = isJust (hasImage (Static dr))
            liftIO . runWithSql . insert $ p
                { pasteResult      = Static dr
                , pasteContainsImg = containsImage
                }
        Right (Interactive dr) -> do
            liftIO . runWithSql . insert $ p
                { pasteResult = Interactive dr }

redirPaste :: Monad m => Int -> ActionT m ()
redirPaste i = redirect $ TL.pack ("/get/" ++ show i)

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


textStrict = text . TL.fromStrict
htmlStrict = html . TL.fromStrict

instance Parsable T.Text where
    parseParam = fmap TL.toStrict . parseParam 

main :: IO ()
main = do
    runWithSql (runMigration migrateAll)
    scottyH 3000 $ do
        -- setTemplatesDir "../common/templates/"
        setHastacheConfig hastacheConf
	middleware $ gzip $ def { gzipFiles = GzipCompress }
        middleware logStdout
        middleware $ staticPolicy (addBase "../common/static")
        S.post "/new" $ eitherT errPage redirPaste (measureTime newPaste)
        S.get "/get/:id" $ maybeT page404 renderPaste getPaste
        S.get "/json/:id" $ maybeT page404 json getPaste
        S.get "/raw/:id/:ind" $ maybeT page404 textStrict getRaw
        S.get "/raw/:id/:ind/pic.svg" $ maybeT page404 htmlStrict getRaw
        S.get "/raw/:id/:ind/all.js" $ maybeT page404 htmlStrict getRaw
        S.get "/gallery" (listImages >>= renderGallery)
        S.get "/" listPastes
        S.get "/feed" feed
	S.get "/about" about
--        S.post "/fetch" $ eitherT errPage redirPaste fetchPaste
