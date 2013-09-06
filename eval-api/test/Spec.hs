{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Applicative
import           Control.Arrow
import           Data.Default
import           Data.Either
import           Data.Monoid
import qualified Data.Text.Lazy                    as TL

import           Unsafe.Coerce

import           Diagrams.Interactive.Eval
import           Diagrams.Interactive.Eval.EvalM
import           Diagrams.Interactive.Eval.Helpers

verbosity :: Int
verbosity = 2

testfile :: FilePath
testfile = "test/test.hs"

runWithFile :: FilePath -> EvalM a -> IO (Either String a)
runWithFile fp a = flip run def $ do
    loadFile fp
    a

main :: IO ()
main = do
  hspec $ sequence_
      [ loadFileSpecs
      , isUnderIOSpecs
      , needsInputSpecs
      ]

loadFileSpecs :: Spec
loadFileSpecs = describe "loadFile" $ do

    it "allows us to inspect the contents of the module" $
        runWithFile testfile (unsafeCoerce <$> compileExpr "test")
            `shouldReturn` Right (2::Int)

    it "returns an error when we try to load a non-existent file" $
        runWithFile "test/nope.hs" (return ()) >>=
            (`shouldSatisfy` isLeft)

isUnderIOSpecs :: Spec
isUnderIOSpecs = describe "isUnderIO" $ do
    it "detects whether the expression is an IO action" $
        runWithFile testfile (isExprUnderIO "undefined :: IO Int")
            `shouldReturn` Right True
    it "detects whether the expression is not an IO action" $
        runWithFile testfile (isExprUnderIO "(*)")
            `shouldReturn` Right False


needsInputSpecs :: Spec
needsInputSpecs = describe "needsInput" $ do
    let needsInput_ = runWithFile testfile . needsInput

    it "does not report on simple non-function datatypes" $
        needsInput_ "undefined :: Int"
            `shouldReturn` Right False

    it "does not report on non-function datatypes" $
        needsInput_ "undefined :: IO Int"
            `shouldReturn` Right False

    it "recognizes simple functions" $
        needsInput_ "undefined :: Int -> String"
            `shouldReturn` Right True

    it "recognizes functions with multiple arguments (specialized to concrete types)" $
        needsInput_ "(+) :: Int -> Int -> Int"
            `shouldReturn` Right True

    it "recognizes functions with multiple arguments (polymorphic types)" $
        needsInput_ "(+) :: Num a => a -> a -> a"
            `shouldReturn` Right True

    it "recognizes functions with multiple arguments (no types given)" $
        needsInput_ "(+)"
            `shouldReturn` Right True

    it "looks under newtypes" $
        needsInput_ "undefined :: MorInt Double"
            `shouldReturn` Right True

    it "looks under nested newtypes" $
        needsInput_ "undefined :: EndoInt"
            `shouldReturn` Right True

    -- | not sure if it's possible at all
    it "looks under datatypes" $
        runWithFile testfile (do
            what <- needsInput "what"
            what2 <- needsInput "what2"
            return (what, what2))
        `shouldReturn` Right (True, True)
