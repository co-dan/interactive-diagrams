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
import Data.Monoid ((<>))    
import Data.Text                    (Text, pack)
import Database.Persist             (Entity)
import Database.Persist.Postgresql  (SqlPersistT, rawSql, toPersistValue)

import Diagrams.Interactive.Display    
import Pastebin.Paste
import Pastebin.Util

data GalleryItem = GalleryItem { itemIndex :: Int, image :: Text }
                 deriving (Data, Typeable)

mkItem :: (Int, PasteGeneric backend) -> GalleryItem
mkItem (k, p) = GalleryItem { itemIndex = k
                            , image = "/raw/" <> k' <> "/" <> ind <> "/pic.svg" }
  where k'  = pack . show $ k
        ind = result $ fromJust . hasImage
              . pasteResult $ p 

gallerySql :: SqlPersistT (LoggingT (ResourceT IO)) [Entity Paste]
gallerySql = rawSql "SELECT ?? FROM \"Paste\" WHERE (\"containsImg\"=?) ORDER BY random() LIMIT 20"
             [toPersistValue True]
