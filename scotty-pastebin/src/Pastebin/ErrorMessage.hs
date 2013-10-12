{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Pastebin.ErrorMessage
    (
      ErrMsg(..)
    , mkErrMsg
    , module Diagrams.Interactive.Eval.EvalError
    ) where

import           Data.Data
import           Data.Foldable                 (foldMap)
import           Data.Monoid                   ((<>))
import           Data.Text.Lazy                (Text, pack)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H

import           Diagrams.Interactive.Eval.EvalError

data ErrMsg = ErrMsg { content :: Text, caption :: String, msgstyle :: String }
            deriving (Typeable, Data)

mkErrMsg :: EvalError -> ErrMsg
mkErrMsg EvalError{..} = ErrMsg
   { content  = renderHtml $
                foldMap ((<> H.br) . H.toHtml . pack) (lines errMsg)
   , caption  = caption
   , msgstyle = style }
  where (style, caption) = case severity of
          SevError   -> ("alert-danger", "Error")
          SevWarning -> ("alert-warning", "Warning")
          SevFatal   -> ("alert-danger", "Error")
          _ -> ("alert-info", "Info")

