{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Diagrams.Interactive.Display.Dynamic.Class where

import           Control.Applicative                  hiding (empty)
import           Control.Monad
import           Control.Monad.Cont
import           Data.Default
import           Data.Foldable
import           Data.Monoid
import           Data.Int
import           Data.Word
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Text.Lazy                       as TL
import           Diagrams.Backend.GHCJS               hiding (Renderable,
                                                       render)
import           Diagrams.Interactive.Display
import           Diagrams.Prelude                     hiding (Renderable,
                                                       render, (<>))
import           GHC.Generics
import           GHCJS.Foreign
import           GHCJS.Types
import           JavaScript.Canvas                    (getContext)
import           JavaScript.JQuery


import           Diagrams.Interactive.Display.Dynamic
import           Diagrams.Interactive.Display.Static

import           Debug.Trace

--------------------------------------------------
-- Main classes

class Inputable a where
    inputable :: JQuery -> ContT JQuery IO (JQuery, IO a)

class Renderable a where
    render :: JQuery -> a -> ContT JQuery IO JQuery
    default render :: Display a => JQuery -> a -> ContT JQuery IO JQuery
    render w = render w . JSDisplay

runRender :: Renderable a => a -> JQuery -> IO JQuery
runRender a w = runContT (render w a) return

--------------------------------------------------

newtype JSDisplay a = JSDisplay a

instance (Display a) => Renderable (JSDisplay a) where
    render w (JSDisplay a) = do
        traceM (show txt)
        area <- lift $ select "<div>"
        lift $ setHtml txt area
        lift $ appendJQuery area w
        return area
        -- ^ if necessary, we will remove this div,
        -- or append additional controll elements to it.
        -- that's why we need to return the inner div
      where
        txt :: T.Text
        txt = displayText (display a)

instance (Inputable a, Renderable b, Show a) => Renderable (a -> b) where
    render w f = do
        (kont, area) <- callCC $ \k -> do
            let f a = k (f, a)
            area <- lift $ select "<div>"
            return (f, area)
        -- ^ we capture the current continuation
        --  in order to return back later
        lift $ empty w
        lift $ appendJQuery area w
        -- ^ whatever `area' we get, we move it to our
        -- "main" DOM element

        -- We append the input controls to the area
        (inputter, getInput) <- inputable area
        lift $ append "<br />\n" area

        -- .. and a "Next" button
        nextBtn <- lift $ appendBtn "Next" area
        lift $ onClick nextBtn $ \_ -> void $ flip runContT return $ do
            -- Upon receiving the "click" event we get the user input
            input <- lift $ getInput
            lift $ putStrLn $ "input: " ++ show input
            -- then we remove our working area completely
            lift $ remove area
            -- .. replacing it with new area with new controls
            buttonBack kont w
            newArea <- render w (f input)
            -- and we put a "back" button on our new area
            buttonBack kont newArea
            return newArea
        return area
      where
        newBtn t = select $ "<button>" <> t <> "</button>"
        appendBtn t place = do
            btn <- newBtn t
            appendJQuery btn place
            return btn
        buttonBack kont area = do
            lift $ append "<br />\n" area
            prevBtn <- lift $ appendBtn "Prev" area
            lift $ click (handlerBack kont area) def prevBtn
        handlerBack kont area _ = void $ flip runContT return $ do
            lift $ remove area
            area' <- lift $ select "<div>"
            kont area'
            return area'

--------------------------------------------------
-- Inputtable instances

instance Inputable String where
    inputable w = do
        inputBox <- lift $ newInputBox
        lift $ appendJQuery inputBox w
        let act = T.unpack <$> getVal inputBox
        return (inputBox, act)
      where
        newInputBox = select "<input type=\"text\" />"

instance Inputable Integer where
    inputable w = do
        (jq, act) <- inputable w
        let act' = read <$> act
        return (jq, act')

--------------------------------------------------
-- Renderable instances

instance Renderable (Diagram Canvas R2) where
    render w d = do
        let nm = "testcanvas"
        canvas <- lift $ select $
                  "<canvas id=\"" <> nm <> "\" width=\"200\" height=\"200\""
                  <> "style=\"border:1px solid #d3d3d3;\">"
                  <> "</canvas><br />"
        ctx <- lift (getContext
                     =<< indexArray 0 (castRef canvas))
        lift $ renderDia Canvas (CanvasOptions (Dims 200 200) ctx) d
        area <- lift $ select "<div>"
        lift $ appendJQuery canvas area
        lift $ appendJQuery area   w
        return area

instance Renderable Int 
instance Renderable Int8 
instance Renderable Int16 
instance Renderable Int32 
instance Renderable Int64 
instance Renderable Word 
instance Renderable Word8 
instance Renderable Word16 
instance Renderable Word32 
instance Renderable Word64 
instance Renderable Integer 
instance Renderable Float 
instance Renderable Double 
instance Renderable Char
instance Renderable ()
instance Display a => Renderable (Maybe a)
instance (Display a, Display b) => Renderable (Either a b)

------------------------------------------------------------
-- Helpers

onClick jq a = click a def jq

displayText (StaticResult drs) =
    foldMap (TL.toStrict . result) drs
