{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, GADTs, EmptyDataDecls #-}
-- I had to put this into a separate module, otherwise TH would complain
module DisplayPersist where

import Database.Persist.TH

import Display
derivePersistField "DisplayResult"

