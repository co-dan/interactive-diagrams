{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Diagrams.Interactive.Display.Dynamic.Class where

import           Control.Applicative                  hiding (empty)
import           Control.Arrow                        ((***))
import           Control.Error
import           Control.Monad
import           Control.Monad.Cont                   hiding (mapM_)
import           Data.Default
import           Data.Foldable                        hiding (find, mapM_)
import           Data.Int
import           Data.List                            (lookup)
import           Data.Maybe
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
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Canvas                    (getContext)
import           JavaScript.JQuery
import           JavaScript.JQuery.UI
import           JavaScript.JQuery.UI.Class
import           JavaScript.JQuery.UI.Internal

import           Diagrams.Interactive.Display.Dynamic
import           Diagrams.Interactive.Display.Static

import           Debug.Trace

--------------------------------------------------
-- Main classes

class Inputable a where
    inputable :: JQuery -> ContT JQuery IO (JQuery, IO (Either String a))
    default inputable :: (Generic a, GInputable (Rep a))
                      => JQuery
                      -> ContT JQuery IO (JQuery, IO (Either String a))
    inputable jq = do
        (jq', act) <- ginputable jq
        jq'' <- lift $ postprocess jq
        return (jq', tto act)
      --   (id *** tto) <$> ginputable jq
      where tto = liftM $ liftM to

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
            pbar <- lift $ select "<div id=\"progressbar\"><div class=\"progresstext\">Munching bits...</div></div>"
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
        let act = return . T.unpack <$> getVal inputBox
        div <- lift $ select "<div>"
        lift $ appendJQuery inputBox div
               >>= appendToJQuery w
        return (div, act)
      where
        newInputBox = select "<input type=\"text\" class=\"input-xmedium\" />"


inputableNum :: (Num a, Read a) => JQuery -> ContT JQuery IO (JQuery, IO (Either String a))
inputableNum = inputableRead (readErr "Cannot read a number")

inputableRead :: (String -> Either String a)
           -> JQuery
           -> ContT JQuery IO (JQuery, IO (Either String a))
inputableRead readF w = do
    (jq, act) <- inputable w
    jq' <- lift $ find "input" jq
    liftIO $ initWidget jq' Spinner with { spinnerPage = 5 }
    let act' = (=<<) <$> pure readF <*> (act :: IO (Either String String))
    return (jq, act')

-- Useful for enums
inputableSelect :: [(T.Text, a)]
                -> JQuery
                -> ContT JQuery IO (JQuery, IO (Either String a))
inputableSelect options w = do
    sel <- lift $ newSelect
    lift $ mapM_ (appendToJQuery sel <=< (mkOpt . fst)) options
    let act = maybe (Left "Unknown option") Right
               .  (`lookup` options)
              <$> getVal sel
    div <- lift $ select "<div>"
    lift $ appendJQuery sel div
           >>= appendToJQuery w
    return (div, act)
  where
    mkOpt s = select "<option>"
              >>= setText s
    newSelect = select "<select>"

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

instance Inputable T.Text  where { inputable = inputableRead (Right . T.pack)  }
instance Inputable TL.Text where { inputable = inputableRead (Right . TL.pack) }

instance Inputable Bool where
    inputable = inputableSelect [("True", True), ("False", False)]
instance Inputable Ordering where
    inputable = inputableSelect [("<", LT), ("=", EQ), (">", GT)]    
instance (Inputable a, Inputable b) => Inputable (Either a b)
instance (Inputable a) => Inputable (Maybe a)

instance (Inputable a, Inputable b) => Inputable (a,b)
instance (Inputable a, Inputable b, Inputable c) => Inputable (a,b,c)

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
instance Renderable String
instance Renderable ()
instance Display a => Renderable (Maybe a)
instance (Display a, Display b) => Renderable (Either a b)
instance (Display a, Display b) => Renderable (a, b)
instance (Display a, Display b, Display c) => Renderable (a,b,c)

------------------------------------------------------------
-- Helpers

onClick jq a = click a def jq

displayText (StaticResult drs) =
    foldMap (TL.toStrict . result) drs

-- * GInputable

type GInputCont f a = ContT JQuery IO (JQuery, IO (Either String (f a)))

class GInputable f where
    ginputable :: JQuery
               -> ContT JQuery IO (JQuery, IO (Either String (f a)))

instance GInputable U1 where
    ginputable jq = do
        codeblock <- lift $ select "<div><code></code></div>"
        lift $ appendJQuery codeblock jq
        return (codeblock, return (Right U1))

instance (Inputable c) => GInputable (K1 i c) where
    ginputable = traceShow "K1" $ liftM (id *** ((fmap . fmap) K1)) . inputable

instance (GInputable f) => GInputable (M1 i c f) where
    ginputable = traceShow "M1" $ liftM (id *** ((fmap . fmap) M1)) . ginputable

instance (GInputable f, GInputable g) => GInputable (f :*: g) where
    ginputable jq = do
        traceM ":*:"
        (inpArea1, inpAct1) <- ginputable jq
        (inpArea2, inpAct2) <- ginputable jq
        -- div <- lift $ select "<div>"
        --             >>= appendJQuery inpArea1
        --             >>= appendJQuery inpArea2
        let act = runEitherT $ do
                res1 <- EitherT inpAct1
                res2 <- EitherT inpAct2
                return (res1 :*: res2)
        return (inpArea1, act)


instance (GInputable f, GInputable g)
         => GInputable (f :+: g) where
    ginputable jq = do
        area <- lift $ do
            sumDiv <- parent jq >>= find ".sum"
            found <- (>0) <$> lengthArray (castRef sumDiv)
            if found
                then return sumDiv
                else newDiv
        (inpArea1, inpAct1) <- ginputable area
        (inpArea2, inpAct2) <- ginputable area
        lift $ initWidget area Accordion def
        let act = do
                (Just n) <- fromJSRef =<< jq_getOptWidget "accordion" "active" area
                divs <- find "div" area
                len <- lengthArray (castRef divs)
                let mid = floor $ ((fromIntegral len)/2) - 1
                traceM $ "Mid = " ++ show mid
                traceM $ "N = " ++ (show n)
                if (n::Int) <= mid
                    then do
                      (n'::JSRef Int) <- toJSRef (n+1)
                      jq_setOptWidget "accordion" "active" n' area
                      liftM L1 <$> inpAct1
                    else do
                      (n'::JSRef Int) <- toJSRef (n-1)
                      jq_setOptWidget "accordion" "active" n' area
                      liftM R1 <$> inpAct2
        return (area, act)
        where newDiv = do
                  accord <- select "<div class=\"sum\">"
                  appendJQuery accord jq
                  return accord


postprocess jq = do
    traceM "postprocess"
    sum <- find ".sum" jq
    find "div" sum
        >>= before "<h3>Option</h3>"
    widgetMethod sum Accordion "destroy"
    initWidget sum Accordion def
    return sum

