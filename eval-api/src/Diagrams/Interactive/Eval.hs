{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | Main entry point of the library
module Diagrams.Interactive.Eval
    (
      module Diagrams.Interactive.Eval.EvalWorker
    , module System.Restricted.Worker
    ) where  

import Diagrams.Interactive.Eval.EvalWorker
import System.Restricted.Worker
