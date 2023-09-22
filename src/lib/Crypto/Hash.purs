module Crypto.Hash (createHash) where

import Prelude

import System.Time (getTimestamp)
import Effect (Effect)

foreign import _createHash :: forall a. a -> String -> Effect String

createHash :: forall a. a -> Effect String
createHash a = do
  tm <- getTimestamp
  _createHash a $ show tm