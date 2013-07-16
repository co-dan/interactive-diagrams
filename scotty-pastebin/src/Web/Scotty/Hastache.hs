{-# LANGUAGE OverloadedStrings #-}
-- | Hastache templating for Scotty
module Web.Scotty.Hastache where

import Web.Scotty as S

import Control.Monad.State
  
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString.Lazy as BL

hastache :: MuConfig ActionM -> FilePath -> MuContext ActionM -> ActionM ()
hastache conf tpl cntx = do
  header "Content-Type" "text/html"
  raw =<< hastacheFile conf tpl cntx
