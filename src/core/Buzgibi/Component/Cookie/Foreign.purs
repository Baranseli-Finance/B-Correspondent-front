module Buzgibi.Component.Cookie.Foreign where

import Prelude

import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack

import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Data.Maybe (Maybe(..))

foreign import set :: BuzgibiBack.Cookie -> Effect Unit
foreign import getIml :: Fn3 (Maybe String) (String -> Maybe String) String (Effect (Maybe String))

get :: String -> Effect (Maybe String)
get = runFn3 getIml Nothing Just