{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Pastebin.Feed
    (
      mkRssFeed
    , renderRss
    , mkRssItem
    ) where

import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.Text               (unpack)
import Data.Time
import Database.Persist        (Entity (..))
import System.Locale
import Text.RSS.Export         (xmlRSS)
import Text.RSS.Syntax         (RSS (..), RSSChannel (..), RSSItem (..),
                                nullChannel, nullItem, nullRSS)
import Text.XML.Light          (showElement)

import Web.Scotty.Trans

import Config
import Pastebin.Coloring
import Pastebin.Paste
import Pastebin.Util

-- | Create a simple RSS feed from the list of pastes
mkRssFeed :: [(Int, Paste)] -> RSS
mkRssFeed pastes = (nullRSS "Pastes" rootPath)
    { rssChannel = (nullChannel "Pastes" rootPath)
                   { rssItems = items
                   }
    }
  where
    items = map (\(k, pst) -> mkRssItem k pst) pastes


-- | Make a single RSS Item based on a paste and its id number
mkRssItem :: Int -> Paste -> RSSItem
mkRssItem pid Paste{..} = (nullItem pasteTitle)
    { rssItemLink        = Just $ rootPath ++ "/get/" ++ show pid
    , rssItemAuthor      = Just $ unpack pasteAuthor
    , rssItemDescription =
        Just $ colorizeStr pasteLiterateHs (unpack pasteContent)
    , rssItemPubDate = Just $ toPubDate pasteCreatedAt
    }
  where toPubDate = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S UT"

-- | Output the RSS feed
renderRss :: Monad m => RSS -> ActionT m ()
renderRss rss = do
    header "Content-Type" "application/rss+xml"
    raw . toLazyByteString . stringUtf8 . showElement . xmlRSS $ rss


