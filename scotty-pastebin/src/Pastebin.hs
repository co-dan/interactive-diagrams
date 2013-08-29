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
import           Control.Monad.State
import           Control.Monad.Trans                  (lift)
import           Control.Monad.Trans.Either           (EitherT (..), eitherT)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Control.Monad.Trans.Resource (transResourceT, ResourceT)
import           Data.Foldable                        (foldMap)
import           Data.Monoid                          (mconcat, mempty)
import           Network                              (PortID (..), connectTo)
import           System.IO                            (hClose)

import           Control.Error.Util                   (hoistMaybe, maybeT,
                                                       nothing)
import           Data.Aeson                           ()
import           Data.EitherR                         (catchT, throwT)
import           Data.Maybe                           (isJust)
import           Data.Text.Lazy                       (Text, pack)
import qualified Data.Text.Lazy                       as T
import           Data.Time.Clock                      (diffUTCTime,
                                                       getCurrentTime)

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Text.Hastache
import           Text.Hastache.Context
import           Web.Scotty.Trans                     as S
import           Web.Scotty.Hastache
import qualified Web.Scotty.Types                     as Scotty
import           Database.Persist                     as P
import           Database.Persist.Sqlite              as P
import           Text.Blaze.Html.Renderer.Text

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
   , muEscapeFunc      = emptyEscape
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
    hastache "main"


renderPaste :: Paste -> ActionH ()
renderPaste (p@Paste{..}) = do
    setH "code"     $ MuVariable pasteContent
    setH "codeView" $ MuVariable (renderCode p)
    setH "title"    $ MuVariable $ mconcat ["Paste / ", T.pack pasteTitle,
                                          " by ", pasteAuthor]
    setH "author"   $ MuVariable pasteAuthor
    setH "ptitle"   $ MuVariable pasteTitle
    setH "literate" $ MuVariable pasteLiterateHs
    setH "result"   $ MuVariable $ case pasteResult of
        Static staticRes ->
            renderHtml $ foldMap renderDR (getDR staticRes)
        Interactive dynRes   ->
            error "dynRes"
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

getRaw :: MaybeT ActionH Text
getRaw = do
    -- pid <- lift $ param "id"
    ind <- lift $ param "ind"
    paste <- getPaste
    case pasteResult paste of
        Static (StaticResult res) ->
            return . result $ res !! ind
        Interactive _ -> nothing

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

feed :: ActionH ()
feed = do
    pastes <- liftIO $ runWithSql $
              selectList [] [LimitTo 20, Desc PasteId]
    renderRss $ mkRssFeed pastes

newPaste :: EitherT (Paste, (Text, [EvalError])) ActionH Int
newPaste = do
    title' <- T.unpack <$> lift (paramEscaped "title")
    let title = if (null title') then "(undefined)" else title'
    code <- lift (param "code")
    usern' <- lift (paramEscaped "author")
    let author = if (T.null usern') then "Anonymous" else usern'
    lhs <- lift (param "literate"
                 `rescue` (return (return False)))
    let p = Paste title code (Static $ StaticResult []) False lhs author
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
            liftIO $ putStrLn "DR"
            liftIO $ print dr
            liftIO $ putStrLn "/DR"
            let containsImage = isJust (hasImage (Static dr))
            liftIO . runWithSql . insert $ p
                { pasteResult      = Static dr
                , pasteContainsImg = containsImage
                }
        Right (Interactive dr) -> do
            error "compilePaste"

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
        middleware . middlewareIO $ logStdoutDev
        middleware . middlewareIO $ staticPolicy (addBase "../common/static")
        S.get "/get/:id" $ maybeT page404 renderPaste getPaste
        S.get "/json/:id" $ maybeT page404 json getPaste
        S.get "/raw/:id/:ind" $ maybeT page404 text getRaw
        S.get "/raw/:id/:ind/pic.svg" $ maybeT page404 html getRaw
        S.get "/gallery" (listImages >>= renderGallery)
        S.post "/new" $ eitherT errPage redirPaste (measureTime newPaste)
        S.get "/" listPastes
        S.get "/feed" feed
--        S.post "/fetch" $ eitherT errPage redirPaste fetchPaste

middlewareIO :: Scotty.Middleware IO
             -> Scotty.Middleware HState
middlewareIO mw app = transResourceT liftIO . mw app1
  where
    app1 :: Scotty.Application IO
    app1 req = transResourceT morph (app req)
    morph :: HState a -> IO a
    morph m = evalStateT m (hastacheConf, mempty)
