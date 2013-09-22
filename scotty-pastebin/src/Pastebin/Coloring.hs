{-# LANGUAGE RecordWildCards #-}
-- Some of the code is taken from the 'hscolour' library
-- by Malcolm Wallace and Bjorn Bringer
module Pastebin.Coloring
    (
      colorize
    , colorizeStr
    ) where

import qualified Data.Text                      as T
import           Language.Haskell.HsColour.CSS

data LitChunk = Code    String
              | Comment String
              deriving (Show)
                       
unChunk :: LitChunk -> String
unChunk (Code    c) = c
unChunk (Comment c) = c


colorize :: Bool     -- ^ Is the code Literate haskell?
         -> T.Text  -- ^ Code to be colourized
         -> T.Text
colorize lhs = T.pack
             . colorizeStr lhs
             . T.unpack

colorizeStr :: Bool -> String -> String
colorizeStr lhs = if lhs
                  then colorizeLhs
                  else hscolour False

colorizeLhs :: String -> String
colorizeLhs = concatMap unChunk
            . map colorizeChunk
            . joinL
            . classify
            . inlines

colorizeChunk :: LitChunk -> LitChunk
colorizeChunk (Comment str) = Comment . wrapPre $ concatMap escape str
colorizeChunk (Code   code) = Code $ hscolour False code

wrapPre :: String -> String
wrapPre s = "<span class=\"lhs-comment\">"
          ++ s
          ++ "</span>"
            
escape :: Char -> String
escape h
    | h == '&'  = "&amp;"
    | h == '\\' = "&#92;"
    | h == '"'  = "&quot;"
    | h == '\'' = "&#39;"
    | h == '<'  = "&lt;"
    | h == '>'  = "&gt;"
    | h == '\n' = "<br />"
    | h == ' '  = "&nbsp;"
    | otherwise = [h]


-- | Join up chunks of code\/comment that are next to each other.
joinL :: [LitChunk] -> [LitChunk]
joinL []                  = []
joinL (Code c:Code c2:xs) = joinL (Code (c++c2):xs)
joinL (Comment c:Comment c2 :xs) = joinL (Comment  (c++c2):xs)
joinL (other:xs)            = other: joinL xs


-- | The code for classify is largely stolen from Language.Preprocessor.Unlit.
classify ::  [String] -> [LitChunk]
classify []             = []
-- classify (x:xs) | "\\begin{code}"`isPrefixOf`x
--                         = Lit x: allProg xs
--    where allProg []     = []  -- Should give an error message,
--                               -- but I have no good position information.
--          allProg (x:xs) | "\\end{code}"`isPrefixOf`x
--                         = Lit x: classify xs
--          allProg (x:xs) = Code x: allProg xs
classify (('>':x):xs)   = Code ('>':x) : classify xs
classify (x:xs)         = Comment x: classify xs


-- Re-implementation of 'lines', for better efficiency (but decreased laziness).
-- Also, importantly, accepts non-standard DOS and Mac line ending characters.
-- And retains the trailing '\n' character in each resultant string.
inlines :: String -> [String]
inlines s = lines' s id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc ['\n'] : lines' s id	-- DOS
--lines' ('\^M':s)      acc = acc ['\n'] : lines' s id	-- MacOS
  lines' ('\n':s)       acc = acc ['\n'] : lines' s id	-- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))

