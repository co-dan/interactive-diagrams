{-# LANGUAGE DeriveGeneric #-}
module Diagrams.Interactive.Display
    (
      module S
    , module D
    , DisplayResult(..)
    -- , displayResult
    ) where

-- import qualified Data.Text.Lazy                       as TL
import Data.Serialize
import Diagrams.Interactive.Display.Dynamic as D
import Diagrams.Interactive.Display.Static  as S
import GHC.Generics


data DisplayResult = Static      StaticResult
                   | Interactive DynamicResult
                   deriving (Generic, Read, Show)

instance Serialize DisplayResult

-- displayResult :: DisplayResult -> TL.Text
-- displayResult (Left str) = S.display str
-- displayResult (Right _)  = error "Display.displayResult"
