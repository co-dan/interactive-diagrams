{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Main where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist as P
import Database.Persist.TH as P
import Database.Persist.Sqlite as P
import Data.Text.Lazy (pack, Text)
import Data.List  
import Control.Applicative
import GHC.Generics
import Data.Maybe
import Display
import DisplayPersist

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Paste
    content     Text
    result      DisplayResult
    containsImg Bool
    deriving Show
|]

share [mkPersist sqlSettings, mkMigrate "migrateOld"] [persistUpperCase|
PasteOld
    content     Text
    result      DisplayResult
    deriving Show
|]

runWithSql :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runWithSql = runResourceT
           . runStdoutLoggingT
           . withSqliteConn "./pastes_old.db"
           . runSqlConn

runWithSqlNew  :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runWithSqlNew = runResourceT
           . runStdoutLoggingT
           . withSqliteConn "./pastes_new.db"
           . runSqlConn

hasImage :: DisplayResult -> Maybe Text
hasImage (DisplayResult drs) =
  result <$> find ((==Display.Svg) . clientType) drs

intToKey :: Int -> Key a
intToKey = Key . PersistInt64 . fromIntegral

keyToInt :: Key a -> Int
keyToInt (Key (PersistInt64 i)) = fromIntegral (toInteger i)

  
old2New :: Entity PasteOld -> Entity Paste
old2New (Entity k p) = Entity k' p'
  where p' = Paste { pasteContent = pasteOldContent p
                   , pasteResult  = pasteOldResult  p
                   , pasteContainsImg = isJust $ hasImage (pasteOldResult p) }
        k' = intToKey . keyToInt $ k

main = do
  runWithSqlNew (runMigration migrateAll)
  (pastesOld :: [Entity PasteOld]) <- runWithSql $ selectList [] []
  let pastesNew = map old2New pastesOld
  mapM_ insertPaste pastesNew

insertPaste :: Entity Paste -> IO ()
insertPaste (Entity k p) = do
  runWithSqlNew $ repsert k p
