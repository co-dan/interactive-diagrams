{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Pastebin.Gallery
    (
      GalleryItem(..)
    , mkItem
    , gallerySql
    ) where

import Control.Monad.Logger         (LoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Data
import Data.Maybe                   (fromJust)
import Data.Text.Lazy               (Text)
import Database.Persist             (Entity)
import Database.Persist.Sqlite      (SqlPersistT, rawSql, toPersistValue)

import Pastebin.Paste
import Pastebin.Util

data GalleryItem = GalleryItem { itemIndex :: Int, image :: Text }
                 deriving (Data, Typeable)

mkItem :: (Int, PasteGeneric backend) -> GalleryItem
mkItem (k, p) = GalleryItem { itemIndex = k
                            , image = fromJust . hasImage
                                      . pasteResult $ p }

gallerySql :: SqlPersistT (LoggingT (ResourceT IO)) [Entity Paste]
gallerySql = rawSql "SELECT ?? FROM \"Paste\" WHERE (\"containsImg\"=?) ORDER BY RANDOM() LIMIT 20"
             [toPersistValue True]
