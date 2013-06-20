{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module EvalError (
  Severity(..), SrcSpan(..),
  EvalError(..),
  LoadingException(..), TooLong(..)
  ) where

import Data.Typeable (Typeable)

import GHC
import Exception

data LoadingException = LoadingException
                      deriving (Typeable)
                               
instance Show LoadingException where
  show LoadingException = "Cannot load the targets"
  
instance Exception LoadingException

data TooLong = TooLong
             deriving (Typeable)
                      
instance Show TooLong where
  show TooLong = "The process has hit the time limit"
  
instance Exception TooLong  

data EvalError = EvalError
    { severity :: Severity     -- ^ The severity of the error
    , errMsg   :: String       -- ^ The error message
    , srcSpan  :: RealSrcSpan  -- ^ Position of the error in the source code
    }

    
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
