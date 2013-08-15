{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Pastebin.Feed
    (
      mkRssFeed
    , renderRss
    , mkRssItem
    ) where

import Data.Text.Lazy          (pack, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Persist
import Text.RSS.Export         (xmlRSS)
import Text.RSS.Syntax         (RSS (..), RSSChannel (..), RSSItem (..),
                                nullChannel, nullItem, nullRSS)
import Text.XML.Light          (showElement)
import Web.Scotty
import Web.Scotty.Types

import Config
import Pastebin.Paste
import Pastebin.Util

-- | Create a simple RSS feed from the list of pastes
mkRssFeed :: [Entity Paste] -> RSS
mkRssFeed pastes = (nullRSS "Pastes" rootPath)
    { rssChannel = (nullChannel "Pastes" rootPath)
                   { rssItems = items
                   }
    }
  where
    items = map (\(Entity k pst) -> mkRssItem (keyToInt k) pst) pastes


-- | Make a single RSS Item based on a paste and its id number
mkRssItem :: Int -> Paste -> RSSItem
mkRssItem pid Paste{..} = (nullItem pasteTitle)
    { rssItemLink    = Just $ rootPath ++ "/get/" ++ show pid
    , rssItemAuthor  = Just $ unpack pasteAuthor
--    , rssItemPubDate =
    }

-- | Output the RSS feed
renderRss :: Monad m => RSS -> ActionT m ()
renderRss rss = do
    header "Content-Type" "application/rss+xml"
    raw . encodeUtf8 . pack . showElement . xmlRSS $ rss


