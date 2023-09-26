module BCorrespondent.Api.Foreign.Auth
  ( AuthApi
  , AuthType(..)
  , NewPassword
  , ResendCode
  , ResponseAuthCode
  , ResponseAuthToken
  , login
  , logout
  , mkAuthApi
  , resendCode
  , resetPasswordLink
  , sendCode
  , setNewPassword
  )
  where

import Prelude

import BCorrespondent.Api.Foreign.Common

import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn3,  Fn2, runFn2, runFn3, Fn4, runFn4)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign)
import Data.Either (Either)
import Effect.Exception as E

foreign import data AuthApi :: Type
foreign import data ResponseAuthToken :: Type
foreign import data ResponseAuthCode :: Type

foreign import mkAuthApi :: Fn1 ApiClient (Effect AuthApi)

type Credentials = 
    { login :: String, 
      password :: String, 
      browserFp :: String 
    }

data AuthType = Jwt

stringifyAuthType :: AuthType -> String
stringifyAuthType Jwt = "jwt"

foreign import _sendCode :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Credentials AuthApi (AC.EffectFnAff (Object ResponseAuthCode))

sendCode :: Credentials -> AuthApi -> AC.EffectFnAff (Object ResponseAuthCode)
sendCode = runFn3 _sendCode withError

type Login = { code :: Int, hash :: String }

foreign import _login :: Fn4 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) String Login AuthApi (AC.EffectFnAff (Object ResponseAuthToken))

login :: Login -> AuthApi -> AC.EffectFnAff (Object ResponseAuthToken)
login = runFn4 _login withError (stringifyAuthType Jwt)

type ResendCode = { browserFp :: String, hash :: String }

foreign import _resendCode :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) ResendCode AuthApi (AC.EffectFnAff (Object ResponseAuthCode))

resendCode :: ResendCode -> AuthApi -> AC.EffectFnAff (Object ResponseAuthCode)
resendCode = runFn3 _resendCode withError

foreign import _logout :: Fn2 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) AuthApi (AC.EffectFnAff (Object Unit))

logout :: AuthApi -> AC.EffectFnAff (Object Unit)
logout = runFn2 _logout withError

foreign import _resetPasswordLink :: Fn2 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) AuthApi (AC.EffectFnAff (Object Foreign))

resetPasswordLink :: AuthApi -> AC.EffectFnAff (Object Foreign)
resetPasswordLink = runFn2 _resetPasswordLink withError

type NewPassword = { key :: String, password :: String }

foreign import _setNewPassword :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) NewPassword AuthApi (AC.EffectFnAff (Object Boolean))

setNewPassword :: NewPassword -> AuthApi -> AC.EffectFnAff (Object Boolean)
setNewPassword = runFn3 _setNewPassword withError