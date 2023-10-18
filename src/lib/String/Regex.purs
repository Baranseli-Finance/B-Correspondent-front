module String.Regex
  ( isNumber
  , isValidNote
  )
  where

import Prelude

foreign import isNumber :: String -> Boolean

foreign import isValidNote :: String -> Boolean
