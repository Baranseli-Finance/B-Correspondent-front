module Halogen.HTML.Properties.Extended
  ( ariaHidden
  , ariaLabel
  , dataDismiss
  , dataLabel
  , ident
  , maxLength
  , module Properties
  , role
  , size
  , tabindex
  )
  where

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

size :: forall r i. Int -> Properties.IProp (size :: Int | r) i
size = Properties.prop (PropName "size")

maxLength :: forall r i. Int -> Properties.IProp (maxLength :: Int | r) i
maxLength = Properties.prop (PropName "maxLength")

-- ident :: forall r i. String -> Properties.IProp (ident :: String | r) i
ident = Properties.prop (PropName "ident")