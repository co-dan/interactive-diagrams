{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Paste where

import Data.Aeson
import Data.Text.Lazy          (Text)
import Database.Persist.TH     as P
import GHC.Generics

import Display
import DisplayPersist ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Paste
    title       String  default="(untitled)"
    content     Text
    result      DisplayResult
    containsImg Bool
    author      Text   default="Anonymous"
    deriving Show
    deriving Generic
|]

instance ToJSON ClientType

instance ToJSON DR where
  toJSON (DR ct r) = object
                     [ "clientType" .= ct
                     , "result"     .= r ]

instance ToJSON DisplayResult

instance ToJSON Paste
