{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module EvalError (
  Severity(..), SrcSpan(..),
  EvalError(..), LoadingException(..)
  ) where

import GHC
import ErrUtils
import Exception
import Data.Typeable

data LoadingException = LoadingException
                      deriving (Typeable)
instance Show LoadingException where
  show LoadingException = "Cannot load the targets"
instance Exception LoadingException


data EvalError = EvalError
                 { severity :: Severity
                 , errMsg   :: String
                 , srcSpan  :: RealSrcSpan }

instance Show EvalError where
  show EvalError{..} =
    unlines
    [ show severity
    , show (srcSpanStartLine srcSpan) ++ ":" ++ show (srcSpanStartCol srcSpan)
      ++ "-" ++
      show (srcSpanEndLine srcSpan) ++ ":" ++ show (srcSpanEndCol srcSpan)
    , errMsg
    ]

  
instance Show Severity where
  show SevWarning = "Warning: "
  show SevError = "Error: "
  show SevFatal = "Fatal: "
  show _ = ""
