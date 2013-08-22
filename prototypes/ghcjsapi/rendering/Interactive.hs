{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module Interactive where

import           Control.Applicative
import           Control.Monad
import           Data.Default
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Text.Lazy               as TL

import           Diagrams.Interactive.Display
import           JavaScript.JQuery
import JavaScript.Canvas (getContext)
import Diagrams.Prelude hiding (Renderable, render, (<>))
import Diagrams.Backend.GHCJS hiding (Renderable, render)
import GHCJS.Foreign
import GHCJS.Types

class Inputable a where
    inputable :: Selector -> IO (JQuery, IO a)

class Renderable a where
    render :: Selector -> a -> IO ()

newtype JSDisplay a = JSDisplay a

instance (Display a) => Display (JSDisplay a) where
    display (JSDisplay a) = display a

instance (Display a) => Renderable (JSDisplay a) where
    render s (JSDisplay a) = do
        area <- select s
        T.putStrLn txt
        void $ setHtml txt area
      where
        txt :: T.Text
        txt = displayText (display a)

displayText (DisplayResult drs) =
    foldMap (TL.toStrict . result) drs

instance Inputable [Char] where
    inputable s = do
        area <- select s
        inputBox <- newInputBox
        jq <- appendJQuery inputBox area
        let act = T.unpack <$> getVal inputBox
        return (jq, act)
      where
        newInputBox = select "<input type=\"text\" />"

instance Inputable Integer where
    inputable s = do
        (jq, act) <- (inputable s :: IO (JQuery, IO String))
        let act' = read <$> act
        return (jq, act')
        
instance (Inputable a, Renderable b, Show a) => Renderable (a -> b) where
    render s f' = do
        (inputRes, getInput) <- inputable s
        area <- select s
        append " " area
        btn <- newBtn
        appendJQuery btn area
        append "<br />\n" area
        click (handler getInput) def btn
        return ()
      where
        f = f'
        newBtn = select "<button>Next</button>"
        handler getInput _ = do
            input <- getInput
            putStrLn $ "input: " ++ show input
            render s (f input)
            select s >>= append "<br/>"
            return ()

instance Renderable (Diagram Canvas R2) where
    render s d = do
        area <- select s
        let nm = "testcanvas"
        let canvas = "<canvas id=\"" <> nm <> "\" width=\"200\" height=\"200\""
                     <> "style=\"border:1px solid #d3d3d3;\">"
                     <> "</canvas><br />"
        setHtml canvas area
        ctx <- getContext =<< indexArray 0 . castRef
               =<< select (s <> " > #" <> nm)
        renderDia Canvas (CanvasOptions (Dims 200 200) ctx) d
