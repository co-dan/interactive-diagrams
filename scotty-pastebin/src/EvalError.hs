{-# LANGUAGE DeriveDataTypeable, RecordWildCards, DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module EvalError (
  Severity(..), SrcPos(..), srcPos,
  EvalError(..),
  LoadingException(..), TooLong(..)
  ) where

import Data.Typeable (Typeable)
import Data.Serialize
import GHC.Generics
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

-- | Position in the source code         
data SrcPos = SrcPos
    { startLine :: Int
    , startCol :: Int
    , endLine :: Int
    , endCol :: Int
    } deriving (Generic)

srcPos :: RealSrcSpan -> SrcPos               
srcPos sp = SrcPos 
  { startLine = srcSpanStartLine sp 
  , startCol = srcSpanStartCol sp
  , endLine = srcSpanEndLine sp
  , endCol = srcSpanEndCol sp }
               
data EvalError = EvalError
    { severity :: Severity     -- ^ The severity of the error
    , errMsg   :: String       -- ^ The error message
    , srcSpan  :: SrcPos  -- ^ Position of the error in the source code
    } deriving (Generic)

deriving instance Generic Severity
instance Serialize Severity
instance Serialize SrcPos  
instance Serialize EvalError
  

instance Show EvalError where
  show EvalError{..} =
    unlines
    [ show severity
    , show (startLine srcSpan) ++ ":" ++ show (startCol srcSpan)
      ++ "-" ++
      show (endLine srcSpan) ++ ":" ++ show (endCol srcSpan)
    , errMsg
    ]

  
instance Show Severity where
  show SevWarning = "Warning: "
  show SevError = "Error: "
  show SevFatal = "Fatal: "
  show _ = ""
