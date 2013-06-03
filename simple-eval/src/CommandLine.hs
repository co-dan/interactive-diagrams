import Control.Monad
import Control.Monad.Trans
import System.IO
import Language.Haskell.Interpreter as I
import Display (DisplayResult(..), DR(..))
import Interp
import SignalHandlers

interp :: String -> I.Interpreter DisplayResult
interp code = do
  I.set [ I.languageExtensions := [ExtendedDefaultRules,NoMonomorphismRestriction] ]
  I.setImports ["Prelude", "Display"]
  I.interpret ("display " ++ I.parens code) I.infer
  
main = forever $ do
  -- scotty 3000 (staticServe >> serve)
  putStr "> "
  hFlush stdout
  code <- getLine
  r <- I.runInterpreter (interp code)
  liftIO $ restoreHandlers
  case r of
    Left err -> print err
    Right (DisplayResult drs) -> print drs
