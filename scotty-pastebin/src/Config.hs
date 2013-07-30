{-# LANGUAGE OverloadedStrings #-}
module Config where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.Sqlite      as P

controlSock :: FilePath
controlSock = "/idia/run/sock/control.sock"

runWithSql :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runWithSql = runResourceT
           . runStdoutLoggingT
           . withSqliteConn "./pastes.db"
           . runSqlConn

getPastesDir :: FilePath
getPastesDir = "/tmp"
