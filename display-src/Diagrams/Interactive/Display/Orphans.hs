{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Diagrams.Interactive.Display.Orphans where

import           Control.Monad      (liftM)
import           Data.Serialize
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL

-- Warning: orphans
instance Serialize TL.Text where
  put = put . TL.encodeUtf8
  get = liftM TL.decodeUtf8 get

instance Serialize T.Text where
  put = put . T.encodeUtf8
  get = liftM T.decodeUtf8 get
