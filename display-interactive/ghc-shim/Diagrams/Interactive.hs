-- shims for the GHC, since we can't install diagrams-interactive as a
-- plain GHC package yet
{-# LANGUAGE NullaryTypeClasses, CPP #-}
module Diagrams.Interactive (Inputable(..), Renderable(..)) where

#ifdef __GHCJS__

import Diagrams.Interactive.Display.Dynamic.Class

#else

class Inputable a where

class Renderable a where

#endif
