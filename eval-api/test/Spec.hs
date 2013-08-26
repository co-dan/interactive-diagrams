{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Hspec
import Test.QuickCheck

import Control.Applicative
import Data.Default
import Data.Monoid
import qualified Data.Text.Lazy as TL

import Diagrams.Interactive.Eval
import Diagrams.Interactive.Eval.Helpers
import Diagrams.Interactive.Eval.EvalM

verbosity :: Int
verbosity = 2

testfile :: FilePath
testfile = "test.hs"

runWithFile :: FilePath -> EvalM a -> IO (Either String a)
runWithFile fp a = flip run def $ do
    loadFile fp
    a

main :: IO ()
main = do
  hspec $ do
    describe "loadFile loads a Haskell source file and bring its module content into the current context" $ do
        it "allows us to inspect the contents of the module" $ do
            pending            
    describe "needsInput determines whether the given expression needs additional input from the user" $ do
        it "does not report on simple datatypes" $
            pending
        it "recognizes simple functions" $
            pending
        it "recognizes functions with multiple arguments" $
            pending
        it "looks under newtypes" $
            pending
        it "looks under datatypes" $
            pending
    
