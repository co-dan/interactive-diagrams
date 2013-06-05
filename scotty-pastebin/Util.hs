{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Util where

import Database.Persist.Sqlite as P

import Control.Monad.Logger
import Control.Monad.Trans.Resource

import Display

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
