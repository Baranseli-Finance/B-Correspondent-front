module Buzgibi.Api.Foreign.File.Api
  ( FileApi
  , mkFileApi
  , upload
  ) where

import Prelude

import Buzgibi.Api.Foreign.Common

import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn4, runFn4)
import Web.File.File (File)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign)
import Data.Either (Either)
import Effect.Exception as E

foreign import data FileApi :: Type

foreign import mkFileApi :: Fn1 ApiClient (Effect FileApi)

foreign import _upload :: Fn4 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) String File FileApi (AC.EffectFnAff (Object (Response (Array Int))))

upload :: String -> File -> FileApi -> (AC.EffectFnAff (Object (Response (Array Int))))
upload file = runFn4 _upload withError file