{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Util (
  controlSock,
  runWithSql,
  getDR,
  intToKey,
  keyToInt,
  hash,
  getPastesDir,
  renderDR,
  hasImage
  ) where

import qualified Data.Hashable as H
import Data.Time.Clock

import Database.Persist.Sqlite as P

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Applicative
import Control.Monad
import Text.Blaze.Svg (Svg)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes (type_, class_, href, rel, action, method,
                                    name, value, cols, rows)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Utf8

import Display

controlSock :: FilePath
controlSock = "/idia/run/sock/control.sock"

runWithSql :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runWithSql = runResourceT
           . runStdoutLoggingT
           . withSqliteConn "./pastes.db"
           . runSqlConn

getDR :: DisplayResult -> [DR]
getDR (DisplayResult drs) = drs
       
intToKey :: Int -> Key a
intToKey = Key . PersistInt64 . fromIntegral

keyToInt :: Key a -> Int
keyToInt (Key (PersistInt64 i)) = fromIntegral (toInteger i)

hash :: H.Hashable a => a -> IO Int
hash a = H.hashWithSalt <$> currentTime <*> pure a

currentTime :: IO Int       
currentTime = getCurrentTime >>= return . floor . toRational . utctDayTime

getPastesDir :: FilePath
getPastesDir = "/tmp"

renderDR :: DR -> H.Html
renderDR (DR Html r) = H.preEscapedToHtml r
renderDR (DR Svg  r) = H.preEscapedToHtml r
renderDR (DR Text r) = H.pre $ H.toHtml r
renderDR (DR RuntimeErr r) = H.div ! HA.class_ "alert alert-error" $
                             H.toHtml r
                               

entityKey :: Entity t -> Key t
entityKey (Entity k' _) = k'

hasImage :: DisplayResult -> Maybe TL.Text
hasImage (DisplayResult drs) =
  result <$> find ((==Display.Svg) . clientType) drs

