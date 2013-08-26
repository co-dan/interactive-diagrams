module Test where

newtype MorInt a = MorInt (a -> Int)

newtype EndoInt = EndoInt { unEnd :: MorInt Int }

test :: Int
test = 2

data TEst = TEst Int
          | TEstFun (Int -> Int)

what :: TEst
what = TEst 1

what2 :: TEst
what2 = TEstFun (*2)

main :: IO Int
main = return 1

