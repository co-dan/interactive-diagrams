{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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
import           Data.Monoid                         (mempty)
import qualified Data.Text.Lazy                      as TL
import           Data.Time.Clock
import           Database.Persist.Sqlite             as P
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Text.Blaze.Html5                    ((!))
import qualified Text.Blaze.Html5                    as H
import qualified Text.Blaze.Html5.Attributes         as HA

import           Web.Scotty                          as S
import           Web.Scotty.Types

import           Diagrams.Interactive.Display        as Display

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
    H.div ! HA.class_ "btn-group" $ do
        H.button ! HA.class_ "btn" ! HA.id "inc" $
            H.i mempty ! HA.class_ "icon-plus"
        H.button ! HA.class_ "btn" ! HA.id "dec" $
            H.i mempty ! HA.class_ "icon-minus"
    H.preEscapedToHtml r
            
renderDR (DR Text r) = H.toHtml r
renderDR (DR RuntimeErr r) = H.div ! HA.class_ "alert alert-error" $
                                 H.toHtml r

renderCode :: TL.Text -> TL.Text
renderCode = TL.pack
           . hscolour CSS defaultColourPrefs False True "Paste" False
           . TL.unpack

