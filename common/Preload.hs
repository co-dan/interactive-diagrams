module Preload (allExceptions, preload, unsafePerformIO, catch, SomeException) where
import Display
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception  
import Control.Applicative ((<$>))
import Diagrams.Prelude
import Diagrams.Backend.SVG

import System.IO.Unsafe  

allExceptions :: SomeException -> IO DisplayResult
allExceptions = return . display

preload :: (Diagram SVG R2, Int)
preload = 
  let d = dia 50
      x = 100 + 100
  in (d, x)

  
dia :: Int -> Diagram SVG R2
dia n = polygon
  with { polyType =
            PolyPolar (repeat (frac n))
            (take n (cycle [1,3,1,4,2,1]))
       } 

frac :: Int -> CircleFrac
frac n = CircleFrac $ fromRational
         (1/(toRational n))
