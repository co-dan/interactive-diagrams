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
    , intToKey
    , keyToInt
      -- * Hashing
    , hash
      -- * Rendering
    , renderCode
    , renderDR
    ) where

import           Control.Applicative
import qualified Data.Hashable                       as H
import           Data.List
import           Data.Monoid                         (mconcat, mempty)
import qualified Data.Text.Lazy                      as TL
import           Data.Time.Clock
import           Database.Persist.Sqlite             as P
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Text.Blaze.Html5                    ((!))
import qualified Text.Blaze.Html5                    as H
import qualified Text.Blaze.Html5.Attributes         as HA

import           Text.Hastache                       (MuVar (..))
import           Web.Scotty                          as S
import           Web.Scotty.Types

import           Diagrams.Interactive.Display        as Display

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
paramEscaped = (fmap . fmap) (TL.concatMap escape) S.param
  where escape :: Char -> TL.Text
        escape h
          | h == '&' = "&amp;"
          | h == '\\'= "&#92;"
          | h == '"' = "&quot;"
          | h == '\''= "&#39;"
          | h == '<' = "&lt;"
          | h == '>' = "&gt;"
          | otherwise     = TL.singleton h


-- * Hashing

currentTime :: IO Int
currentTime = getCurrentTime >>= return . floor . toRational . utctDayTime

hash :: H.Hashable a => a -> IO Int
hash a = H.hashWithSalt <$> currentTime <*> pure a

-- * Rendering

renderDR :: DR -> H.Html
renderDR (DR Html r) = H.preEscapedToHtml r
renderDR (DR Svg  r) = H.div ! HA.class_ "thumbnail" $ do
    -- btngrp
    H.preEscapedToHtml img
  where
    btngrp = H.div ! HA.class_ "btn-group" $ do
        H.button ! HA.class_ "btn btn-mini" ! HA.id "inc" $
            H.i mempty ! HA.class_ "icon-plus"
        H.button ! HA.class_ "btn btn-mini" ! HA.id "dec" $
            H.i mempty ! HA.class_ "icon-minus"
    img = mconcat [ "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"100%\" height=\"400\" class=\"csvg\">"
                  , "<g id=\"viewport\">"
                  , r
                  , "</g></svg>" ]


renderDR (DR Text r) = H.toHtml r
renderDR (DR RuntimeErr r) = H.div ! HA.class_ "alert alert-error" $
                                 H.toHtml r

renderCode :: Paste -> TL.Text
renderCode Paste{..} = TL.pack
           $ hscolour CSS defaultColourPrefs False True "Paste" pasteLiterateHs
           $ TL.unpack pasteContent


----------------------------------------
-- orphan instance

instance MuVar Bool where
    isEmpty = not
    toLByteString True  = "True"
    toLByteString False = "False"
