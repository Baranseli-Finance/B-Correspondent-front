module BCorrespondent.Api.Foreign.Auth
  ( AuthApi
  , AuthType(..)
  , ResponseAuthToken
  , login
  , mkAuthApi
  ) where

import Prelude

import BCorrespondent.Api.Foreign.Common

import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn4, runFn4)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign)
import Data.Either (Either)
import Effect.Exception as E

foreign import data AuthApi :: Type
foreign import data ResponseAuthToken :: Type

foreign import mkAuthApi :: Fn1 ApiClient (Effect AuthApi)

type Credentials = { email :: String, password :: String }

data AuthType = Jwt

stringifyAuthType :: AuthType -> String
stringifyAuthType Jwt = "jwt"

foreign import _login :: Fn4 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) String Credentials AuthApi (AC.EffectFnAff (Object ResponseAuthToken))

login :: Credentials -> AuthApi -> (AC.EffectFnAff (Object ResponseAuthToken))
login = runFn4 _login withError (stringifyAuthType Jwt)