{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Errors that may arise during the evaluation
module Diagrams.Interactive.Eval.EvalError
    (
      Severity(..)
    , SrcPos(..)
    , srcPos
    , EvalError(..)
    , LoadingException(..)
    , TooLong(..)
    ) where

import Data.Serialize
import Data.Typeable  (Typeable)
import Exception
import GHC
import GHC.Generics

-- | The targets cannot be loaded    
data LoadingException = LoadingException
                      deriving (Typeable)

instance Show LoadingException where
    show LoadingException = "Cannot load the targets"

instance Exception LoadingException

-- | It's taking too long for the process to complete         
data TooLong = TooLong
             deriving (Typeable)

instance Show TooLong where
    show TooLong = "The process has hit the time limit"

instance Exception TooLong

-- | Position in the source code
data SrcPos = SrcPos
    { startLine :: Int
    , startCol  :: Int
    , endLine   :: Int
    , endCol    :: Int
    } deriving (Generic, Eq, Read)

srcPos :: RealSrcSpan -> SrcPos
srcPos sp = SrcPos
    { startLine = srcSpanStartLine sp
    , startCol = srcSpanStartCol sp
    , endLine = srcSpanEndLine sp
    , endCol = srcSpanEndCol sp }

-- | Evaluation error    
data EvalError = EvalError
    { severity :: Severity  -- ^ The severity of the error
    , errMsg   :: String    -- ^ The error message
    , srcSpan  :: SrcPos    -- ^ Position of the error in the source code
    } deriving (Generic, Eq, Read)
               
deriving instance Generic Severity
deriving instance Eq Severity
deriving instance Read Severity
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

