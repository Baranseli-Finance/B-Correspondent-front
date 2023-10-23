module BCorrespondent.Scroll ( onDetectVisibile ) where

import Prelude

import Effect (Effect)
import Web.DOM.Element (Element)

import Undefined

onDetectVisibile :: Element -> Element -> Effect Boolean
onDetectVisibile _ _ = pure false 
     

