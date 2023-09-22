module Halogen.HTML.Properties.Extended
  ( ariaHidden
  , ariaLabel
  , dataDismiss
  , dataLabel
  , module Properties
  , role
  , tabindex
  ) where

import Prelude

import Halogen.HTML.Properties as Properties
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..))

-- role :: forall r i. String -> Properties.IProp (role :: String | r) i
role = Properties.prop (PropName "role")
ariaLabel = Properties.prop (PropName "aria-label")
dataDismiss = Properties.prop (PropName "data-dismiss")
ariaHidden = Properties.prop (PropName "aria-hidden")
dataLabel = Properties.prop (PropName "data-label")
tabindex = Properties.prop (PropName "tabindex")

