module BCorrespondent.Scroll ( onDetectVisibile ) where

import Prelude

import Effect (Effect)
import Web.DOM.Element (Element)

import Undefined


foreign import _onDetectVisible :: Element -> Element -> Effect Boolean

onDetectVisibile :: Element -> Element -> Effect Boolean
onDetectVisibile = _onDetectVisible 
     

