{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Diagrams.Interactive.Display.Dynamic where

import           Data.Serialize
import qualified Data.Text.Lazy                       as TL
import           GHC.Generics

import           Diagrams.Interactive.Display.Orphans ()

newtype DynamicResult = DynamicResult TL.Text
                      deriving (Generic, Show, Read)


instance Serialize DynamicResult
