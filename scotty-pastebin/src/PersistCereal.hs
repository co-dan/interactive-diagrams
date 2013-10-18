{-# LANGUAGE OverloadedStrings #-}
module PersistCereal where

import           Data.Serialize
import qualified Data.Text           as T
import           Database.Persist

toPersistValue :: Serialize a => a -> PersistValue
toPersistValue = PersistByteString . encode

fromPersistValue :: Serialize a => PersistValue -> Either T.Text a
fromPersistValue (PersistByteString bs) = either (Left . T.pack) Right (decode bs)
fromPersistValue _ = Left "Serializable values must be converted from PersistByteString"
    
