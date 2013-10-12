{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE CPP                        #-}
module Diagrams.Interactive.Display.Dynamic where

import           Control.Applicative                  hiding (empty)
import           Control.Arrow                        ((***))
import           Control.Error
import           Control.Monad
import           Control.Monad.Cont                   hiding (mapM_)
import           Data.Default
import           Data.Foldable                        hiding (find, mapM_)
import           Data.Function
import           Data.Either
import           Data.Int
import           Data.IORef
import           Data.List                            (lookup,sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Serialize                       hiding (Result) 
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Data.Word
import           Diagrams.Prelude                     hiding (Renderable, Result,
                                                       render, with, (<>))
import           GHC.Generics

import           Diagrams.Interactive.Display.Static
import           Diagrams.Interactive.Display.Orphans ()

#if __GHCJS__
    
import           Diagrams.Backend.GHCJS               hiding (Renderable,
                                                       render)
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Canvas                    (getContext)
import           JavaScript.JQuery                    hiding (on)
import qualified JavaScript.JQuery                    as JQ
import           JavaScript.JQuery.UI
import           JavaScript.JQuery.UI.Class
import           JavaScript.JQuery.UI.Internal
    
#endif    

import           Debug.Trace


newtype DynamicResult = DynamicResult T.Text
                      deriving (Generic, Show, Read, Monoid)

instance Serialize DynamicResult

-- | The end result of a function
type family Result x where
  Result (a -> b) = Result b
  Result a        = a

#if __GHCJS__

--------------------------------------------------
-- Main classes

-- | Values of type 'a' can be 'inputted'
class Input a where
  input :: JQuery     -- container;
        -> IO (IO (Either String a))  -- outer IO: prepare the container/form,
                      -- inner IO: get input
  default input :: (Generic a, GInput (Rep a))
                => JQuery -> IO (IO (Either String a))
  input w = do
      area <- select "<div class=\"input-group generic\">"
              >>= appendToJQuery w
      act <- ginput area
      postprocess area
      return $ liftM to `fmap` act
  

  inputList :: JQuery -> IO (IO (Either String [a]))
  default inputList :: (Output a)
                    => JQuery -> IO (IO (Either String [a]))
  inputList = defInputList

-- | Values of type 'a' can be 'outputted'
class Output a where
  output :: JQuery          -- container;
         -> IO (JQuery, a -> IO ()) -- outer IO: prepare the container
                                    -- a -> IO () -- update the output
  default output :: (Display a)
                 => JQuery -> IO (JQuery, a -> IO ())
  output w = output w >>= \(a, f) -> return (a, f . JSDisplay)  

-- | 'Interctive a b' means that it's possible to
-- interctively execute 'a' to reach 'b'
class (Result a ~ b) => Interactive a b where
  interactive :: JQuery -> IO (Either String a) -> IO (IO (Either String b))

-- | If we can reach 'c' from 'b' and if it's possible to input 'a',
-- then we can reach 'c' from '(a -> b)'
instance (Input a, Interactive b c) => Interactive (a -> b) c where
  interactive env f = do
    a <- input env
    -- f :: IO (String + (a -> b))
    -- ap :: m (a -> b) -> (m a -> m b)
    -- fmap ap f :: IO (String + a -> String + b)
    interactive env ((fmap ap f) <*> a) 

-- | Base case        
instance (Result a ~ a) => Interactive a a where
  interactive env x = return x

mkInteractiveWidgetTest :: (Interactive a b, Result a ~ b, Output b)
                        => a -> IO ()
mkInteractiveWidgetTest = mkInteractiveWidget "#test"

mkInteractiveWidget :: (Interactive a b, Result a ~ b, Output b)
                    => T.Text -> a -> IO ()
mkInteractiveWidget div k = do
    test <- select div
    _ <- empty test
    act <- runInteractive test k
    btn <- select "<button>Go</button>" >>= appendToJQuery test
    onClick btn $ \_ -> do
        act
    return ()


runInteractive :: (Interactive a b, Result a ~ b, Output b)
               => JQuery -> a -> IO ( IO () )
runInteractive env f = do    
    val       <- interactive env (return (Right f))
    (area, o) <- output env
    return $ do
        --- XXX: display a progress bar
        -- hideEffect area Blind with { blindDirection = "down" }
        v <- val
        case v of
            Left str -> do
                let msg = mconcat [ "<div class=\"alert alert-danger\">"
                                  , T.pack str
                                  , "</div>" ]
                void $ setHtml msg area
            Right b  -> o b

newtype JSDisplay a = JSDisplay a

instance (Display a) => Output (JSDisplay a) where
    output w = do
        area <- select "<div>" >>= appendToJQuery w
        let act = \(JSDisplay a) -> void $ do
            setText (txt a) area
            wrapInner "<pre>" area
        return (area, act)
      where
        txt a = displayText (display a)


--- XXX: ability to delete elements from the list
defInputList :: (Output a, Input a)
             => JQuery
             -> IO (IO (Either String [a]))
defInputList jq = do
    area <- select "<div class=\"input-group\">" >>= appendToJQuery jq
    msgarea <- select "<p>" >>= appendToJQuery area
    listUl <- select "<ul class=\"sortable\">"
              >>= appendToJQuery area
    initWidget listUl Sortable def
    inpAct <- input area
    addBtn <- newBtn "Add"
               >>= appendToJQuery area
    listData <- newIORef (0::Int, []) -- list size, list itself
    onClick addBtn $ \_ -> do
        setText "" msgarea
        res <- inpAct
        case res of
            Left str -> void $
                let errmsg = "<font color=red>" <> (T.pack str) <> "</font>"
                in setHtml errmsg msgarea
            Right a -> void $ addItem a listUl listData
    let -- act :: IO (Either String [a])
        act = do
            (positions :: [Int]) <- mapM (liftM fromJust . fromJSRef . castRef)
                         =<< fromArray . castRef
                         =<< widgetMethod listUl Sortable "toArray"
            (_, elems) <- readIORef listData
            let lst = map fst
                    $ sortBy (compare `on` (Down . snd))
                    $ zip elems positions                    
            return (Right lst)
    return act
  where
    newBtn t = select $ "<button>" <> t <> "</button>"
    appendBtn t place = do
            btn <- newBtn t
            appendJQuery btn place
            initWidget btn Button with { buttonLabel = t }
            return btn
    addItem a ul dat = do
        (n, elems) <- readIORef dat
        li <- select ("<li class=\"ui-state-default\" id=\""
                             <> T.pack (show n)
                             <> "\">")
                   >>= appendToJQuery ul
        span <- select "<span class=\"ui-icon ui-icon-arrowthick-2-n-s\">"
                      >>= appendToJQuery li
        writeIORef dat (n+1, a:elems)
        join $ (snd `fmap` output li) `ap` (return a)

-- --------------------------------------------------
-- -- Inputtable instances

inputString w = do
    inputBox <- newInputBox
    let act = return . T.unpack <$> getVal inputBox
    div <- select "<div class=\"input-group\">"
    appendJQuery inputBox div
        >>= appendToJQuery w
    return act
  where
    newInputBox = select "<input type=\"text\" class=\"input-xmedium\" />"

inputRead :: (String -> Either String a)
          -> JQuery
          -> IO (IO (Either String a))
inputRead readF w = do
    act <- inputString w
    let act' = (=<<) <$> pure readF <*> (act :: IO (Either String String))
    return act'

inputNum :: (Num a, Read a)
         => JQuery -> IO (IO (Either String a))
inputNum w = do
    inputBox <- newInputBox
    let act = readErr "Cannot read a number"
              .  T.unpack
             <$> getVal inputBox
    div <- select "<div class=\"input-group\">"
    appendJQuery inputBox div
        >>= appendToJQuery w
    initWidget inputBox Spinner with { spinnerPage = 5 }
    return act
  where
    newInputBox = select "<input type=\"text\" class=\"input-xmedium\" />"

-- Useful for enums
inputSelect :: [(T.Text, a)]
            -> JQuery
            -> IO (IO (Either String a))
inputSelect options w = do
    sel <- newSelect
    mapM_ (appendToJQuery sel <=< (mkOpt . fst)) options
    let act = maybe (Left "Unknown option") Right
               .  (`lookup` options)
              <$> getVal sel
    div <- select "<div class=\"input-group\">"
    appendJQuery sel div
        >>= appendToJQuery w
    return (act)
  where
    mkOpt s = select "<option>"
              >>= setText s
    newSelect = select "<select>"

instance Input Char where
    input     = inputRead (headErr "Cannot read a char")                
    inputList = inputString

instance (Input a, Output a, Display a) => Input [a] where
    input = inputList

instance Input Int     where { input = inputNum }
instance Input Int8    where { input = inputNum }
instance Input Int16   where { input = inputNum }
instance Input Int32   where { input = inputNum }
instance Input Int64   where { input = inputNum }
instance Input Word    where { input = inputNum }
instance Input Word8   where { input = inputNum }
instance Input Word16  where { input = inputNum }
instance Input Word32  where { input = inputNum }
instance Input Word64  where { input = inputNum }
instance Input Integer where { input = inputNum }
instance Input Float   where { input = inputNum }
instance Input Double  where { input = inputNum }

instance Input T.Text  where { input = inputRead (Right . T.pack)  }
instance Input TL.Text where { input = inputRead (Right . TL.pack) }

---  Enum instances
instance Input () where
    input = inputSelect [("()", ())]
instance Input Bool where
    input = inputSelect [("True", True), ("False", False)]
instance Input Ordering where
    input = inputSelect [("<", LT), ("=", EQ), (">", GT)]    

instance (Input a, Input b,
          Display a, Display b) => Input (Either a b)
instance (Input a, Display a) => Input (Maybe a) where
    input w = do
        -- append "<div class=\"col-lg-6\">" w
        area   <- select "<div class=\"input-group\">"
                  >>= appendToJQuery w
        spn    <- select "<span class=\"input-group-addon\">"
                  >>= appendToJQuery area
        lbl    <- select "<label>"
                  >>= appendToJQuery spn
        append "Just?" lbl
        inner  <- select "<div>" >>= appendToJQuery area
        inpAct <- input inner
        cbox   <- select "<input type=\"checkbox\" />"
                  >>= setAttr "checked" "true"
                  >>= appendToJQuery lbl
        let h _ = void $ do
                checked <- is ":checked" cbox
                if checked
                    then setAttr "readonly" "false" inner
                    else setAttr "readonly" "true"  inner
        JQ.on h "change" def cbox
        return $ do
            checked <- is ":checked" cbox
            if checked
                then liftM (liftM Just) inpAct
                else return (return Nothing)
        
instance (Input a, Input b,
          Display a, Display b,
          Output a, Output b)
         => Input (a,b)
instance (Input a, Input b, Input c,
          Display a, Display b, Display c,
          Output a, Output b, Output c)
         => Input (a,b,c)

-- --------------------------------------------------
-- -- Outputable instances

instance (b ~ Canvas) => Output (Diagram b R2) where
    output w = do
        let nm = "testcanvas"
        area <- select "<div>"
        appendJQuery area w
        let act d = do
            empty area
            canvas <- select
                      ("<canvas id=\"" <> nm <> "\" width=\"200\" height=\"200\""
                       <> "style=\"border:1px solid #d3d3d3;\">"
                       <> "</canvas><br />")
                      >>= appendToJQuery area
            -- setAttr "width" "400" canvas
            ctx <- getContext
                   =<< indexArray 0 (castRef canvas)
            renderDia Canvas (CanvasOptions (Dims 200 200) ctx) d
        return (area, act)

instance Output Int
instance Output Int8
instance Output Int16
instance Output Int32
instance Output Int64
instance Output Word
instance Output Word8
instance Output Word16
instance Output Word32
instance Output Word64
instance Output Integer
instance Output Float
instance Output Double
instance Output Char
instance Output T.Text
instance Output TL.Text
instance Output ()

instance (Output a, Display a) => Output [a]

instance Output Bool
instance Output Ordering
instance Display a => Output (Maybe a)
instance (Display a, Display b) => Output (Either a b)
instance (Display a, Display b)
         => Output (a, b)
instance (Display a, Display b, Display c) => Output (a,b,c)

-- ------------------------------------------------------------
-- -- Helpers

onClick jq a = click a def jq

displayText (StaticResult drs) = foldMap result drs

-- -- -- * GInput & generic instances

class GInput f where
    ginput :: JQuery
           -> IO (IO (Either String (f a)))

instance GInput U1 where
    ginput jq = do
        codeblock <- select "<div><code></code></div>"
        appendJQuery codeblock jq
        return (return (Right U1))

instance (Input c) => GInput (K1 i c) where
    ginput = liftM ((fmap . fmap) K1) . input

instance (GInput f) => GInput (M1 i c f) where
    ginput = liftM ((fmap . fmap) M1) . ginput

instance (GInput f, GInput g) => GInput (f :*: g) where
    ginput jq = do
        area <- select "<div>" >>= appendToJQuery jq
        inpAct1 <- ginput area
        inpAct2 <- ginput area
        let act = runEitherT $ do
                res1 <- EitherT inpAct1
                res2 <- EitherT inpAct2
                return (res1 :*: res2)
        return act


instance (GInput f, GInput g)
         => GInput (f :+: g) where
    ginput jq = do
        area <- do
            sumDiv <- parent jq >>= childrenMatching ".sum"
            found <- (>0) <$> lengthArray (castRef sumDiv)
            if found
                then return sumDiv
                else newDiv
        inpAct1 <- ginput area
        inpAct2 <- ginput area
        initWidget area Accordion def
        --- XXX: can we do this without chaging the active accordion panel?
        let act = do
                (Just n) <- fromJSRef =<< jq_getOptWidget "accordion" "active" area
                divs <- childrenMatching "div" area
                len <- lengthArray (castRef divs)
                let mid = floor $ ((fromIntegral len)/2) - 1
                let main = if (n::Int) <= mid
                           then do
                               (n'::JSRef Int) <- toJSRef (n+1)
                               jq_setOptWidget "accordion" "active" n' area
                               liftM L1 <$> inpAct1
                           else do
                               (n'::JSRef Int) <- toJSRef (n-1)
                               jq_setOptWidget "accordion" "active" n' area
                               liftM R1 <$> inpAct2
                nOrig <- toJSRef n
                main <* jq_setOptWidget "accordion" "active" nOrig area
        return act
        where newDiv = do
                  accord <- select "<div class=\"sum\">"
                  appendJQuery accord jq
                  return accord


-- | XXX: this is a hack
postprocess jq = do
    sum <- find ".sum" jq
    childrenMatching "div" sum
        >>= before "<h3>Option</h3>"
    widgetMethod sum Accordion "destroy"
    initWidget sum Accordion def
    return sum    
    
#else

class Input a where
class Output a where

runInteractive = error "runInteractive available only in JavaScript"
mkInteractiveWidgetTest = error "mkInteractiveWidgetTest available only in JavaScript"
mkInteractiveWidget = error "mkInteractiveWidget available only in JavaScript"
#endif    
