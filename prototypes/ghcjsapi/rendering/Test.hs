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

import Factorization
-- ^ http://hub.darcs.net/alp/factorization-diagrams-happstack
import Interactive

data JSDChunk = forall a . Display a => JSDChunk (JSDisplay a)

instance Renderable JSDChunk where
    render s (JSDChunk c) = render s c

c :: forall a . Display a => a -> JSDChunk
c = JSDChunk . JSDisplay

main = do
    let cs = [c ("Hello, world" :: String), c ("<br />" :: String), c (123 :: Int)]
    mapM_ (render ".yo") cs    
    render "#test" f

displayText (DisplayResult drs) =
    foldMap (TL.toStrict . result) drs

f :: Integer -> Integer -> Diagram Canvas R2
f x y = factorDiagram (x+y)
