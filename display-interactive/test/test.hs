{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE DeriveGeneric             #-}
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
-- import Factorization
-- ^ http://hub.darcs.net/alp/factorization-diagrams-happstack
-- import Interactive
import Diagrams.Interactive.Display.Dynamic.Class
import GHC.Generics

data Wot = A String | B Int | C Int 
         deriving (Generic, Show)

instance Inputable Wot 

f :: Wot -> Int
f (A s) = length s
f (B i) = i
f (C _) = 0

data Foo = Foo String
         deriving (Generic, Show)

instance Inputable Foo

foo :: Foo -> String
foo (Foo s) = s

data Bar = Bar String Int
         deriving (Generic, Show)

instance Inputable Bar

g :: Bar -> Int
g (Bar a b) = b + length a

main = do
    test <- select "#test"
    _ <- runRender f test
    return ()

