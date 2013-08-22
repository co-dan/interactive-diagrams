{-# LANGUAGE FlexibleContexts #-}
-- code by Alp Mestanogullari
-- http://hub.darcs.net/alp/factorization-diagrams-happstack
-- Copyright (c) 2012, Alp Mestanogullari <alpmestan@gmail.com>
module Factorization where
 
import Math.NumberTheory.Primes.Factorisation (factorise)

import Diagrams.Prelude
import Diagrams.Backend.GHCJS

primeLayout :: Integer -> Diagram Canvas R2 -> Diagram Canvas R2
primeLayout 2 d
  | width d > height d = d === strutY (height d / 2) === d
  | otherwise          = d ||| strutX (width d / 2)  ||| d
primeLayout p d = decoratePath pts (repeat d)
  where pts = polygon with { polyType   = PolyRegular (fromIntegral p) r
                           , polyOrient = OrientH
                           }
        w = max (width d) (height d)
        r = w * c / sin (tau / (2 * fromIntegral p))
        c = 0.75

factorDiagram' :: [Integer] -> Diagram Canvas R2
factorDiagram' []     = circle 1 # fc black
factorDiagram' (p:ps) = primeLayout p (factorDiagram' ps) # centerXY

factorDiagram :: Integer -> Diagram Canvas R2
factorDiagram = factorDiagram'
              . reverse
              . concatMap (uncurry $ flip replicate)
              . factorise
