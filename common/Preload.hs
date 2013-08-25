module Preload (allExceptions, preload, unsafePerformIO, catch, SomeException) where
import Diagrams.Interactive.Display
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception  
import Control.Applicative ((<$>))
import Diagrams.Prelude
import Diagrams.Backend.SVG

import System.IO.Unsafe  

allExceptions :: SomeException -> IO DisplayResult
allExceptions = return . Left . display

preload :: (Diagram SVG R2, Int)
preload = 
  let d = circle 1
      x = 100 + 100
  in (d, x)

  
