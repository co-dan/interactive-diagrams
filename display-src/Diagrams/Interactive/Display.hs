{-# LANGUAGE DeriveGeneric #-}
module Diagrams.Interactive.Display
    (
      module S
    , module D
    , DisplayResult(..)
    ) where

import Data.Serialize
import Diagrams.Interactive.Display.Dynamic as D
import Diagrams.Interactive.Display.Static  as S
import GHC.Generics


data DisplayResult = Static      StaticResult
                   | Interactive DynamicResult
                   deriving (Generic, Read, Show)

instance Serialize DisplayResult


