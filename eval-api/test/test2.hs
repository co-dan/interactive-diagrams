{-# LANGUAGE FlexibleContexts #-}
module Main (main, example) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

main = return ()

example :: Integer -> Diagram SVG R2
example x = circle (fromInteger x)
