module Buzgibi.Page.User.Survey.File (removeValue) where

import Prelude

import Web.DOM.Internal.Types (Element)
import Effect (Effect)

foreign import removeValue :: Element -> Effect Unit