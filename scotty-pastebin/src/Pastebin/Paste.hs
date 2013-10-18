{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Pastebin.Paste where

import Data.Aeson
import Data.Text               (Text)
import Data.Time.Clock
import Database.Persist        as P
import Database.Persist.TH     as P
import GHC.Generics

import Diagrams.Interactive.Display
import Diagrams.Interactive.Eval.EvalError
import Pastebin.DisplayPersist ()

import PersistCereal as S

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Paste
    title       String default='(untitled)'
    content     Text
    result      DisplayResult
    errors      [EvalError] default='[]'
    containsImg Bool
    literateHs  Bool   default=False
    author      Text
    createdAt   UTCTime
    parent      PasteId Maybe
    deriving Show
    deriving Generic
|]

instance ToJSON ClientType

instance ToJSON DR where
  toJSON (DR ct r) = object
                     [ "clientType" .= ct
                     , "result"     .= r ]

instance ToJSON DisplayResult
instance ToJSON StaticResult
instance ToJSON DynamicResult
instance ToJSON Paste

-- EvalError ToJSON orphan
instance ToJSON Severity
instance ToJSON SrcPos
instance ToJSON EvalError


instance PersistField EvalError where
    toPersistValue   = S.toPersistValue
    fromPersistValue = S.fromPersistValue
