module Eval.EvalSettings
    (
     EvalSettings(..), defaultSettings
    ) where

import Data.Default

-- | Datastructure holding the settings for the interpreter  
data EvalSettings = EvalSettings
    { -- | Path to the directory where temporary files are held
      tmpDirPath  :: FilePath
      -- | File name that will be used for source code.
      -- The result will be written to '<filename>.res'
    , fileName    :: FilePath
      -- | Maximum time for which the code is allowed to run
      -- (in seconds)
    , timeout     :: Int
    }

    
defaultSettings :: EvalSettings
defaultSettings = EvalSettings
    { tmpDirPath = "/tmp"
    , fileName   = "test.hs"
    , timeout    = 3
    }

instance Default EvalSettings where
  def = defaultSettings