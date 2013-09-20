{-# LANGUAGE NullaryTypeClasses, CPP #-}
module Diagrams.Interactive
    ( Inputable(..)
    , Renderable(..)
    , Display(..)
    ) where

import Diagrams.Interactive.Display.Static

#ifdef __GHCJS__

import Diagrams.Interactive.Display.Dynamic

#else

class Inputable a where

class Renderable a where

#endif

