{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Pastebin.Util
    (
      -- * Convertation & queries
      getDR
    , hasImage
    , paramEscaped
    , paramMaybe
    , intToKey
    , keyToInt
      -- * Hashing
    , hash
      -- * Rendering
    , renderCode
    , renderDR
    ) where

import           Control.Applicative
import qualified Data.Hashable                as H
import           Data.List
import           Data.Monoid                  (mconcat, mempty)
import qualified Data.Text.Lazy               as TL
import           Data.Time.Clock
import           Database.Persist.Postgresql  as P
import           Text.Blaze.Html5             ((!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as HA

import           Text.Hastache                (MuVar (..))
import           Web.Scotty                   as S
import           Web.Scotty.Trans             as ST

import           Diagrams.Interactive.Display as Display

import           Pastebin.Coloring
import           Pastebin.Paste

-- * Convertation & quering

hasImage :: DisplayResult -> Maybe DR
hasImage (DisplayResult drs) =
    find ((==Display.Svg) . clientType) drs

getDR :: DisplayResult -> [DR]
getDR (DisplayResult drs) = drs

intToKey :: Int -> Key a
intToKey = Key . PersistInt64 . fromIntegral

keyToInt :: Key a -> Int
keyToInt (Key (PersistInt64 i)) = fromIntegral (toInteger i)
keyToInt (Key _)                = error "Unknown key format"

paramEscaped :: (Monad m, Functor m) => TL.Text -> ActionT m TL.Text
paramEscaped = (fmap . fmap) (TL.concatMap escape) ST.param

paramMaybe :: (Monad m, Functor m, Parsable a) => TL.Text -> ActionT m (Maybe a)
paramMaybe q = fmap Just (ST.param q)
               `ST.rescue` \_ -> return Nothing

escape :: Char -> TL.Text
escape h
    | h == '&'  = "&amp;"
    | h == '\\' = "&#92;"
    | h == '"'  = "&quot;"
    | h == '\'' = "&#39;"
    | h == '<'  = "&lt;"
    | h == '>'  = "&gt;"
    | otherwise = TL.singleton h


-- * Hashing

currentTime :: IO Int
currentTime = getCurrentTime >>= return . floor . toRational . utctDayTime

hash :: H.Hashable a => a -> IO Int
hash a = H.hashWithSalt <$> currentTime <*> pure a

-- * Rendering

renderDR :: DR -> H.Html
renderDR (DR Html r) = H.preEscapedToHtml r
renderDR (DR Svg  r) = H.div ! HA.class_ "thumbnail" $
    H.preEscapedToHtml img
  where
    img = mconcat [ "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"100%\" height=\"400\" class=\"csvg\">"
                  , "<g id=\"viewport\">"
                  , r
                  , "</g></svg>" ]


renderDR (DR Text r) = H.toHtml r
renderDR (DR RuntimeErr r) = H.div ! HA.class_ "alert alert-error" $
                                 H.toHtml r

renderCode :: Paste -> TL.Text
renderCode Paste{..} = colorize pasteLiterateHs pasteContent

----------------------------------------
-- orphan instance

instance MuVar Bool where
    isEmpty = not
    toLByteString True  = "True"
    toLByteString False = "False"
