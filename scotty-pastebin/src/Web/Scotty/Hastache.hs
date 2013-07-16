{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Hastache templating for Scotty
module Web.Scotty.Hastache where

import Web.Scotty as S

import Control.Monad.State

import Data.Typeable

import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString.Lazy as BL

import Text.Blaze.Html5 ((!))
import Text.Blaze.Internal
import Text.Blaze.Html5.Attributes (type_, class_, href, rel, action, method,
                                    name, value, cols, rows)
import Text.Blaze.Html.Renderer.Utf8 as BRU
import Text.Blaze.Html.Renderer.String as BRS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Display

hastache :: MuConfig IO -> FilePath -> MuContext IO -> ActionM ()
hastache conf tpl cntx = do
  header "Content-Type" "text/html"
  res <- liftIO $ hastacheFile conf tpl cntx
  raw res

-- deriving instance Show H.Html
-- instance Show H.Html where
--   show = BRS.renderHtml

instance MuVar Markup where
  isEmpty = isEmpty . BRU.renderHtml
  toLByteString = BRU.renderHtml
