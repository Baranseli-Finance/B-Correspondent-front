module Date.Format (format) where

import Prelude
import Effect (Effect)

foreign import format :: String -> Effect String

