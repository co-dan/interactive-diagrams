{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Util (
  controlSock,
  runWithSql,
  getDR,
  intToKey,
  keyToInt,
  hash,
  getPastesDir
  ) where

import qualified Data.Hashable as H
import Data.Time.Clock

import Database.Persist.Sqlite as P

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Applicative
import Control.Monad

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

