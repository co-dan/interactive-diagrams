{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- This has to be a separate module, otherwise GHC would complain
module Pastebin.DisplayPersist where

import Database.Persist.TH
import Diagrams.Interactive.Display

derivePersistField "DisplayResult"
