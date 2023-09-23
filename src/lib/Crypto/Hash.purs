module Crypto.Hash
  ( Algo(..)
  , createHash
  )
  where

import Prelude

import System.Time (getTimestamp)
import Effect (Effect)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

foreign import _createHash :: forall a. String -> a -> String -> Effect String

data Algo = SHA256 | SHA512

derive instance Generic Algo _

instance showLog :: Show Algo where
  show = genericShow

createHash :: forall a. Algo -> a -> Effect String
createHash algo a = do
  tm <- getTimestamp
  _createHash (show algo) a $ show tm