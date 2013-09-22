{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveGeneric        #-}
module Diagrams.Interactive.Display.Orphans where

import           Control.Monad                 (liftM)
import           Data.Serialize
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import qualified Diagrams.Prelude              as D
import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5              as B

-- Warning: orphans
instance Serialize TL.Text where
  put = put . TL.encodeUtf8
  get = liftM TL.decodeUtf8 get

instance Serialize T.Text where
  put = put . T.encodeUtf8
  get = liftM T.decodeUtf8 get

instance Show B.Markup where
  show = TL.unpack . renderHtml

instance Show (D.QDiagram b v m) where
  showsPrec _ _ = showString "<diagram>"
