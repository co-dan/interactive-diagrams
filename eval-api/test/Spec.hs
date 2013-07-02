{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Hspec
import Test.QuickCheck

import Control.Applicative
import Data.Default
import Data.Monoid
import qualified Data.Text.Lazy as TL

import Eval
import Display
import Eval.EvalM
import Eval.EvalSettings
import Eval.Helpers  
import Eval.EvalError

eval :: EvalQueue -> EvalM DisplayResult -> IO (String, [EvalError])
eval q act = do
  (r, errors) <- sendEvaluator q act
  case r of
    Right (DisplayResult res) ->
      return (mconcat (map (TL.unpack . result) res), errors)
    Left err -> return (err, errors)

cmpexpr :: String -> EvalM DisplayResult
cmpexpr e = compileExpr $ "(return $ display (" ++ e ++ ")) :: IO DisplayResult"

main :: IO ()
main = do
  (q, _) <- prepareEvalQueue $ def { secontext = Nothing }
  hspec $ do
    describe "compileExpr" $ do
      it "compiles integers and returns `DisplayResult's sucessfully" $
        property $ \(x :: Integer) -> do
          eval q (cmpexpr (show x ++ " :: Integer")) 
            `shouldReturn` (show x, [])
      it "compiles arithmetic expressions and returns `DisplayResult's sucessfully" $ 
        eval q (cmpexpr "123 * 5 :: Int")
          `shouldReturn` ("615", [])
      it "compiles string expressions and returns `DisplayResult's sucessfully" $ 
        fst <$> eval q (cmpexpr "\"hello\" ++ \"world\"")
          `shouldReturn` "\"helloworld\""
      
