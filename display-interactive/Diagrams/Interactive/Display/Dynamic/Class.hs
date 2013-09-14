{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Diagrams.Interactive.Display.Dynamic.Class where

import           Control.Applicative                  hiding (empty)
import           Control.Error.Safe
import           Control.Monad
import           Control.Monad.Cont
import           Data.Default
import           Data.Foldable
import           Data.Int
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Text.Lazy                       as TL
import           Data.Word
import           Diagrams.Backend.GHCJS               hiding (Renderable,
                                                       render)
import           Diagrams.Interactive.Display
import           Diagrams.Prelude                     hiding (Renderable,
                                                       render, with, (<>))
import           GHC.Generics
import           GHCJS.Foreign
import           GHCJS.Types
import           JavaScript.Canvas                    (getContext)
import           JavaScript.JQuery
import           JavaScript.JQuery.UI
import           JavaScript.JQuery.UI.Class

import           Diagrams.Interactive.Display.Dynamic
import           Diagrams.Interactive.Display.Static

import           Debug.Trace

--------------------------------------------------
-- Main classes

class Inputable a where
    inputable :: JQuery -> ContT JQuery IO (JQuery, IO (Either String a))

class Renderable a where
    render :: JQuery -> a -> ContT JQuery IO JQuery
    default render :: Display a => JQuery -> a -> ContT JQuery IO JQuery
    render w = render w . JSDisplay

runRender :: Renderable a => a -> JQuery -> IO JQuery
runRender a w = runContT (render w a) return

runRenderTest :: Renderable a => a -> IO ()
runRenderTest a = do
    w <- select "#test"
    _ <- runRender a w
    return ()

--------------------------------------------------

newtype JSDisplay a = JSDisplay a

instance (Display a) => Renderable (JSDisplay a) where
    render w (JSDisplay a) = do
        traceM (show txt)
        area <- lift $ select "<div>"
        lift $ setText txt area
        lift $ wrapInner "<code>" area
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
            inputErr <- lift $ getInput
            --  we also jump back in the time if we can't parse input
            input <- case inputErr of
                Right x  -> return x
                Left err -> do
                    let errmsg = "Error getting input: " ++ err
                    lift $ putStrLn errmsg
                    lift $ remove area
                    area' <- lift $ select "<div>"
                    lift $ append ("<p><font color=red>" <> T.pack errmsg <> "</font></p>")
                                  area'                    
                    kont area'
                    return undefined
            lift $ putStrLn $ "input: " ++ show input

            -- Show the "loading.." message
            pbar <- lift $ select "<div id=\"progressbar\">Muching bits</div>"
            lift $ empty area
            lift $ appendJQuery pbar area -- replace the inside of an
                -- area with our progressbar                
            lift $ initWidget pbar Progressbar with { progressbarValue = F }
            
            
            -- .. replacing the old area with the new area with new controls
            newArea <- render w (f input)
            -- then we remove our working area completely
            lift $ remove area
            
            -- and we put a "back" button on our new area
            buttonBack kont newArea
            return newArea
        return area
      where
        newBtn t = select $ "<button>" <> t <> "</button>"
        appendBtn t place = do
            btn <- newBtn t
            appendJQuery btn place
            initWidget btn Button with { buttonLabel = t }
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
        let act = return . T.unpack <$> getVal inputBox
        return (inputBox, act)
      where
        newInputBox = select "<input type=\"text\" class=\"input-xmedium\" />"


inputableNum :: (Num a, Read a) => JQuery -> ContT JQuery IO (JQuery, IO (Either String a))
inputableNum w = do
    (jq, act) <- inputable w
    liftIO $ initWidget jq Spinner with { spinnerPage = 5 }
    let errmsg = "Cannot read an number"
    let act' = (=<<) <$> pure (readErr errmsg) <*> (act :: IO (Either String String))
    return (jq, act')

instance Inputable Int     where { inputable = inputableNum }
instance Inputable Int8    where { inputable = inputableNum }
instance Inputable Int16   where { inputable = inputableNum }
instance Inputable Int32   where { inputable = inputableNum }
instance Inputable Int64   where { inputable = inputableNum }
instance Inputable Word    where { inputable = inputableNum }
instance Inputable Word8   where { inputable = inputableNum }
instance Inputable Word16  where { inputable = inputableNum }
instance Inputable Word32  where { inputable = inputableNum }
instance Inputable Word64  where { inputable = inputableNum }
instance Inputable Integer where { inputable = inputableNum }
instance Inputable Float   where { inputable = inputableNum }
instance Inputable Double  where { inputable = inputableNum }

--------------------------------------------------
-- Renderable instances

instance (b ~ Canvas) => Renderable (Diagram b R2) where
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
