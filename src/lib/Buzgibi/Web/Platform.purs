module Buzgibi.Web.Platform (getPlatform) where

import Prelude

import Data.Function.Uncurried (Fn1)
import Effect (Effect)

foreign import getPlatform :: Fn1 String (Effect String)