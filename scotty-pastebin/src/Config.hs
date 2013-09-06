{-# LANGUAGE OverloadedStrings #-}
module Config where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.Postgresql    as P

controlSock :: FilePath
controlSock = "/idia/run/sock/control.sock"

connStr :: ConnectionString
connStr = "host=localhost port=5432 user=idia dbname=idia password=idia_password"

runWithSql :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runWithSql = runResourceT
           . runStdoutLoggingT
           . withPostgresqlConn connStr
           . runSqlConn

getPastesDir :: FilePath
getPastesDir = "/tmp"

getTemplatesDir :: FilePath
getTemplatesDir = "../common/templates/"

-- | The website URL, without the trailing slash
rootPath :: String
rootPath = "http://localhost:3030"
