module Diagrams.Interactive.Display
    (
      module S
    , module D
    , DisplayResult
    -- , displayResult
    ) where

-- import qualified Data.Text.Lazy                       as TL
import           Diagrams.Interactive.Display.Dynamic as D
import           Diagrams.Interactive.Display.Static  as S


type DisplayResult = Either StaticResult DynamicResult

-- displayResult :: DisplayResult -> TL.Text
-- displayResult (Left str) = S.display str
-- displayResult (Right _)  = error "Display.displayResult"
