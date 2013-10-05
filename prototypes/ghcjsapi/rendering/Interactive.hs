{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module Interactive where

import           Control.Applicative hiding (empty)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Cont
import           Data.Default
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Text.Lazy               as TL

import           Diagrams.Backend.GHCJS       hiding (Renderable, render)
import           Diagrams.Interactive.Display.Static
import           Diagrams.Prelude             hiding (Renderable, render, (<>), Result)
import           GHCJS.Foreign
import           GHCJS.Types
import           JavaScript.Canvas            (getContext)
import           JavaScript.JQuery

import Debug.Trace


type family Result x where
  Result (a -> b) = Result b
  Result a        = a

-- class In a where
--     in_  :: JQuery -> IO (IO a)

-- class Out b where
--     out_ :: JQuery -> IO (b -> IO ())

-- class I a b where
--     i_   :: JQuery -> a -> IO b
    
-- instance (Show a, Out a) => I a a where
--     i_ env a = do
--         traceM "I a a"
--         out' <- out_ env
--         btn <- select "<button>Go</button>" >>= appendToJQuery env        
--         onClick btn $ \_ -> putStrLn "HELLO" >> (traceShow a $ out' a)
--         return a
         
-- instance (In a, I b c) => I (a -> b) c where
--     i_ env f = do
--         traceM "I a->b c"
--         getA <- in_ env
--         fp <- f <$> getA
--         i_ env fp

onClick jq a = click a def jq

-- instance In String where
--     in_ env = do
--         putStrLn "ttest"
--         inputBox <- newInputBox
--         appendJQuery inputBox env
--         let act = T.unpack <$> getVal inputBox
--         return act


-- instance In Int where
--     in_ env = liftM read <$> in_ env

-- instance Out Int where
--     out_ env = do
--         print "test"
--         div <- select "<div>" >>= appendToJQuery env        
--         return $ \a -> void $ setText (T.pack . show $ a) div

-- runI' :: (Show b, Result a ~ b, Out b, I a b) => JQuery -> a -> IO b
-- runI' env f = i_ env f

-- runI :: (Show b, Result a ~ b, Out b, I a b) => JQuery -> a -> IO ()
-- runI env f = void $ runI' env f


class Input a where
  input :: JQuery     -- container
        -> IO ()      -- "updated" callback
        -> IO (IO a)  -- outer IO: prepare the container/form,
            -- inner IO - get input 

class Output a where
  output :: JQuery     -- container
         -> IO (a -> IO ()) -- outer IO: prepare the container
            -- IO () -- update the output
         
class Interactive a b | a -> b where
  interactive :: JQuery -> IO a -> IO () -> IO (IO b)

instance (Input a, Interactive b c) => Interactive (a -> b) c where
  interactive env f upd = do
    a <- input env upd
    interactive env (f `ap` a) upd
      
instance (Show a) => Interactive a a where
  interactive env x upd = return x

runInteractive :: (Show b, Result a ~ b, Output b, Interactive a b) => JQuery -> a -> IO ( IO () )
runInteractive env f = do
    val <- interactive env (return f) (return ())
    o   <- output env
    return (val >>= o)

instance Input String where
    input env upd = do
        inputBox <- newInputBox
        appendJQuery inputBox env
        let act = T.unpack <$> getVal inputBox
        return act

newInputBox = select "<input type=\"text\" />"

instance Input Int where
    input env upd = liftM read <$> input env upd

instance Output Int where
    output env = do
        div <- select "<div>" >>= appendToJQuery env
        return $ \a -> void $ setText (T.pack . show $ a) div

-- -- instance (Display a) => Display (JSDisplay a) where
-- --     display (JSDisplay a) = display a

-- -- instance (Display a) => Renderable (JSDisplay a) where
-- --     render w (JSDisplay a) = do
-- --         traceM (show txt)
-- --         area <- lift $ select "<div>" 
-- --         lift $ setHtml txt area
-- --         lift $ appendJQuery area w
-- --         return area
-- --         -- ^ if necessary, we will remove this div,
-- --         -- or append additional controll elements to it.
-- --         -- that's why we need to return the inner div
-- --       where
-- --         txt :: T.Text
-- --         txt = displayText (display a)

-- -- displayText (StaticResult drs) =
-- --     foldMap result drs

-- -- instance Inputable [Char] where
-- --     inputable w = do
-- --         inputBox <- lift $ newInputBox
-- --         lift $ appendJQuery inputBox w
-- --         let act = T.unpack <$> getVal inputBox
-- --         return (inputBox, act)
-- --       where
-- --         newInputBox = select "<input type=\"text\" />"

-- -- instance Inputable Integer where
-- --     inputable w = do
-- --         (jq, act) <- inputable w
-- --         let act' = read <$> act
-- --         return (jq, act')

-- -- onClick jq a = click a def jq

-- -- instance (Inputable a, Renderable b, Show a) => Renderable (a -> b) where
-- --     render w f = do
-- --         (kont, area) <- callCC $ \k -> do
-- --             let f a = k (f, a)
-- --             area <- lift $ select "<div>"
-- --             return (f, area)
-- --         -- ^ we capture the current continuation
-- --         --  in order to return back later

-- --         traceM "rendering.."
-- --         lift $ empty w
-- --         lift $ appendJQuery area w
-- --         -- ^ whatever `area' we get, we move it to our
-- --         -- "main" DOM element
        
-- --         -- We append the input controls to the area
-- --         (inputter, getInput) <- inputable area
-- --         lift $ append "<br />\n" area

-- --         -- .. and a "Next" button
-- --         nextBtn <- lift $ appendBtn "Next" area
-- --         lift $ onClick nextBtn $ \_ -> void $ flip runContT return $ do
-- --             -- Upon receiving the "click" event we get the user input
-- --             input <- lift $ getInput
-- --             lift $ putStrLn $ "input: " ++ show input
-- --             -- then we remove our working area completely
-- --             lift $ remove area
-- --             -- .. replacing it with new area with new controls
-- --             newArea <- render w (f input)
-- --             -- and we put a "back" button on our new area
-- --             buttonBack kont newArea
-- --             return newArea
-- --         return area
-- --       where
-- --         newBtn t = select $ "<button>" <> t <> "</button>"
-- --         appendBtn t place = do
-- --             btn <- newBtn t
-- --             appendJQuery btn place
-- --             return btn
-- --         buttonBack kont area = do
-- --             lift $ append "<br />\n" area
-- --             prevBtn <- lift $ appendBtn "Prev" area
-- --             lift $ click (handlerBack kont area) def prevBtn
-- --         handlerBack kont area _ = void $ flip runContT return $ do
-- --             lift $ remove area
-- --             area' <- lift $ select "<div>"
-- --             kont area'
-- --             return area'

-- -- instance Renderable (Diagram Canvas R2) where
-- --     render w d = do
-- --         let nm = "testcanvas"
-- --         canvas <- lift $ select $
-- --                   "<canvas id=\"" <> nm <> "\" width=\"200\" height=\"200\""
-- --                   <> "style=\"border:1px solid #d3d3d3;\">"
-- --                   <> "</canvas><br />"                
-- --         ctx <- lift (getContext
-- --                      =<< indexArray 0 (castRef canvas))                
-- --         lift $ renderDia Canvas (CanvasOptions (Dims 200 200) ctx) d
-- --         area <- lift $ select "<div>"
-- --         lift $ appendJQuery canvas area
-- --         lift $ appendJQuery area   w
-- --         return area
