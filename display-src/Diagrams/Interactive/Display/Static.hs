{- Original file from GHCLive project,
copyright (c) 2012, Shae Erisson, Edward Kmett, Luite Stegeman
          (c) 2013, Dan Frumin, Luite Stegeman -}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Diagrams.Interactive.Display.Static
    (
      -- * Main datatypes
      StaticResult(..)
    , ClientType(..)
    , DR(..)
    , Display(..)
      -- * Helper functions
    , html
    , text
    , svg
    , displayString
    , displayChar
      -- * Other functions
    , displaying
    , displayEmpty
    ) where

import           Prelude                       hiding (span)

import           Control.Exception             (SomeException)
import           Data.Int
import           Data.Monoid
import           Data.Serialize
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Typeable
import           Data.Word
import qualified Diagrams.Backend.SVG          as D
import qualified Diagrams.Prelude              as D
import           GHC.Generics
import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5              as B

import Diagrams.Interactive.Display.Orphans ()


-- * Main datatypes
    
data ClientType = Html | Svg | Text | RuntimeErr
                deriving (Eq, Show, Enum, Read, Generic)


newtype StaticResult = StaticResult [DR]
                      deriving (Eq, Monoid, Typeable,
                                Show, Read, Generic)
data DR = DR {
      clientType :: ClientType, -- "SVG" "IMG" etc, changes how the browser-side javascript handles this result.
      result     :: T.Text            -- actual result data
      } deriving (Eq, Show, Typeable, Read, Generic)
                 
instance Serialize ClientType
instance Serialize StaticResult
instance Serialize DR

text :: T.Text -> StaticResult
text x = StaticResult [ DR Text x ]

html :: B.Markup -> StaticResult
html x = StaticResult [ DR Html (TL.toStrict (renderHtml x)) ]

svg :: B.Markup -> StaticResult
svg x = StaticResult [ DR Svg (TL.toStrict (renderHtml x)) ]

displayString :: String -> StaticResult
displayString = text . T.pack

displayChar :: Char -> StaticResult
displayChar = displayString . return

displayListOf :: (a -> StaticResult) ->  [a] -> StaticResult
displayListOf _     []     = displayString "[]"
displayListOf showx (x:xs) = displayChar '[' <> showx x <> showl xs
  where
    showl []     = displayChar ']'
    showl (y:ys) = displayChar ',' <> showx y <> showl ys

-- | Too fool ExtendedDefaultRules into firing
displaying :: (Display a, Show a) => a -> StaticResult
displaying = display

class GDisplay f where
  gdisplay :: f a -> StaticResult

instance GDisplay U1 where
  gdisplay U1 = mempty

instance Display a => GDisplay (K1 i a) where
  gdisplay (K1 a) = display a

instance (GDisplay f, GDisplay g) => GDisplay (f :+: g) where
  gdisplay (L1 f) = gdisplay f
  gdisplay (R1 g) = gdisplay g

instance (GDisplay f, GDisplay g) => GDisplay (f :*: g) where
  gdisplay (f :*: g) = gdisplay f <> displayChar ' ' <> gdisplay g

instance (Constructor c, GDisplay f) => GDisplay (M1 C c f) where
  gdisplay m@(M1 x) = displayString (conName m) <> displayChar ' ' <> gdisplay x

instance GDisplay f => GDisplay (M1 S c f) where
  gdisplay (M1 x) = gdisplay x

instance GDisplay f => GDisplay (M1 D c f) where
  gdisplay (M1 x) = gdisplay x

class Display a where
    displayList :: [a] -> StaticResult
    displayList = displayListOf display

    display :: a -> StaticResult
    default display :: (Generic a, GDisplay (Rep a)) => a -> StaticResult
    display = gdisplay . from

displayEmpty :: StaticResult
displayEmpty = StaticResult [] 

renderMyDiagramToSvg :: Double -> D.QDiagram D.SVG D.R2 Any -> B.Html
renderMyDiagramToSvg size = D.renderDia D.SVG (D.SVGOptions (D.Dims size size))

instance Display StaticResult where
  display d = d

instance Display SomeException where
  display e = StaticResult [
    DR RuntimeErr (T.pack . ("Uncaught runtime error: " ++) . show $ e)
    ]

instance (a ~ D.SVG, b ~ D.R2, c ~ Any) => Display (D.QDiagram a b c) where
  display     = svg . renderMyDiagramToSvg 400
  displayList = displayListOf (svg . renderMyDiagramToSvg 200)

instance Display TL.Text where
  display d = displayChar '"' <> text (TL.toStrict d) <> displayChar '"'

instance Display T.Text where
  display d = displayChar '"' <> text d <> displayChar '"'

instance Display a => Display [a] where
  display = displayList

instance Display B.Markup where
  display = html

instance (Display a, Display b) => Display (a,b) where
  display (a, b) = displayChar '(' <> display a <> displayChar ',' <> display b <> displayChar ')'

instance (Display a, Display b, Display c) => Display (a,b,c) where
  display (a, b, c) = displayChar '(' <> display a <> displayChar ',' <> display b <> displayChar ',' <> display c <> displayChar ')'

instance Display Int where display = displayString . show
instance Display Int8 where display = displayString . show
instance Display Int16 where display = displayString . show
instance Display Int32 where display = displayString . show
instance Display Int64 where display = displayString . show
instance Display Word where display = displayString . show
instance Display Word8 where display = displayString . show
instance Display Word16 where display = displayString . show
instance Display Word32 where display = displayString . show
instance Display Word64 where display = displayString . show
instance Display Integer where display = displayString . show
instance Display Float where display = displayString . show
instance Display Double where display = displayString . show
instance Display Char where
  display = displayString . show
  displayList = display . T.pack
instance Display () where display () = displayString "()"

-- generic instances
instance Display Bool
instance Display Ordering
instance Display a => Display (Maybe a)
instance (Display a, Display b) => Display (Either a b)
