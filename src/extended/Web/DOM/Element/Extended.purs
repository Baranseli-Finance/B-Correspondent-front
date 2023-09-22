module Web.DOM.Element.Extended (remove, module Element) where

import Prelude

import Web.DOM.Element as Element
import Effect

foreign import remove :: Element.Element -> Effect Unit
