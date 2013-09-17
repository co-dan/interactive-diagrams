{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
module Main where

import Control.Monad
import qualified Data.Text.Lazy as TL
import Data.Foldable (foldMap)
import Diagrams.Interactive.Display
import Diagrams.Prelude hiding (Renderable, render)
import Diagrams.Backend.GHCJS
import JavaScript.JQuery
import GHCJS.Prim

import Debug.Trace
import Factorization
-- ^ http://hub.darcs.net/alp/factorization-diagrams-happstack
-- import Interactive
import Diagrams.Interactive.Display.Dynamic.Class

a = JSRef undefined

renderF = void . runRender f

testMe = runRender (5::Int)

main = do
    test <- select "#test"
    _ <- runRender f test
    return ()


loop :: Integer -> ()
loop x = loop x
    
f :: Integer -> Integer -> Diagram Canvas R2
f x y = factorDiagram (x+y)

x :: Int -> Int -> IO ()
x a b = traceM $ show (a+b)

g :: Integer -> Integer -> JSDisplay Integer
g x y = JSDisplay $ x + y
