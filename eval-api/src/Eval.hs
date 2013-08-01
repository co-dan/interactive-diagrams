{-# LANGUAGE ScopedTypeVariables, RankNTypes, DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | Main entry point of the library
module Eval
    (
      module Eval.EvalWorker
    , module Worker
    ) where  

import Eval.EvalWorker
import Worker
