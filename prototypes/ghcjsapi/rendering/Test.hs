{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
module Main where

import qualified Data.Text.Lazy as TL
import Data.Foldable (foldMap)
import Diagrams.Interactive.Display
import Diagrams.Prelude hiding (Renderable, render)
import Diagrams.Backend.GHCJS
import JavaScript.JQuery

import Factorization
-- ^ http://hub.darcs.net/alp/factorization-diagrams-happstack
import Interactive


main = do
    test <- select "#test"
    _ <- runRender f test
    return ()

displayText (DisplayResult drs) =
    foldMap (TL.toStrict . result) drs

f :: Integer -> Integer -> Diagram Canvas R2
f x y = factorDiagram (x+y)

g :: Integer -> Integer -> JSDisplay Integer
g x y = JSDisplay $ x + y
