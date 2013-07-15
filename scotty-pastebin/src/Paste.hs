{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Paste where

import Data.Aeson
import Data.Typeable
import Data.Text.Lazy (pack, Text)
import Database.Persist as P
import Database.Persist.TH as P
import Database.Persist.Sqlite as P
import GHC.Generics
  
import Display
import DisplayPersist

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Paste
    content Text
    result DisplayResult
    deriving Show
    deriving Typeable
    deriving Generic
|]

instance ToJSON ClientType

instance ToJSON DR where
  toJSON (DR ct r) = object
                     [ "clientType" .= ct
                     , "result"     .= r ]

instance ToJSON DisplayResult

instance ToJSON Paste         
