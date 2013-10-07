{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE DeriveGeneric             #-}
import JavaScript.JQuery
import Diagrams.Interactive.Display.Dynamic hiding (onClick)
import Diagrams.Interactive.Display.Static
import Diagrams.Backend.GHCJS
import Diagrams.Prelude
import Data.Default
import GHC.Generics

onClick jq a = click a def jq

data Foo = Foo String Int
         | Bar Bool (Int, Int)
         | Baz Ordering String 
         | Booz ()
         deriving (Generic, Show)

instance Display Foo
instance Output Foo
instance Input Foo

main = do
    test <- select "#test"
    act <- runInteractive test k
    btn <- select "<button>Go</button>" >>= appendToJQuery test
    onClick btn $ \_ -> do
        act
    return ()

f :: Foo -> Int
f (Foo _ _) = 0
f (Bar _ _) = 1
f (Baz _ _) = 2
f (Booz _ ) = 3

g :: Int -> Maybe String -> Foo -> Int
g x (Just s) (Bar True _) = x + length s
g _ _ _ = -1 

k :: () -> Double -> Diagram Canvas R2
k () n = circle 1 <> square n
