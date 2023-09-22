module Buzgibi.Api.Foreign.Common
  ( ApiClient
  , Error
  , JWTStatus(..)
  , JWTToken(..)
  , Response
  , Success(..)
  , fetchWS
  , getDataFromObj
  , getDataFromObjWS
  , mkApiClient
  , withError
  ) where

import Prelude

import Effect.Exception as E
import Effect
import Foreign.Object (Object)
import Data.Either
import Data.Function.Uncurried (Fn1, Fn2, runFn2)
import Foreign (Foreign)
import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Web.Socket (WebSocket)
import Effect.Aff.Compat as AC

import Undefined

foreign import data ApiClient :: Type
foreign import data Response :: Type -> Type
foreign import data Error :: Type

instance showError :: Show Error where
  show = _printError

foreign import _printError :: Error -> String

type Success a = { success :: a, warnings :: Array String }

foreign import _getDataFromObj
  :: forall a b
   . (String -> Either E.Error b)
  -> (Success b -> Either E.Error (Success b))
  -> Object a
  -> Effect (Either E.Error b)

getDataFromObj :: forall a b. Object a -> Effect (Either E.Error (Success b))
getDataFromObj = _getDataFromObj (Left <<< E.error) Right

foreign import _getDataFromObjWS
  :: forall a b
   . (String -> Either E.Error b)
  -> (Success b -> Either E.Error (Success b))
  -> Object a
  -> Effect (Either E.Error b)

getDataFromObjWS :: forall a b. Object a -> Effect (Either E.Error (Success b))
getDataFromObjWS = _getDataFromObjWS (Left <<< E.error) Right

foreign import _mkApiClient :: Fn2 String String (Effect ApiClient)

mkApiClient :: Maybe String -> String -> Effect ApiClient
mkApiClient jwt = runFn2 _mkApiClient (fromMaybe undefined jwt)

foreign import withError :: forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a

newtype JWTToken = JWTToken String

instance Show JWTToken where
  show (JWTToken token) = "***token***"

instance EncodeJson JWTToken where
  encodeJson (JWTToken token) = "token" := token ~> jsonEmptyObject

data JWTStatus = Valid | Invalid | Skip

derive instance Generic JWTStatus _

instance DecodeJson JWTStatus where
  decodeJson = genericDecodeJson

foreign import _fetchWS :: forall a. Fn2 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) WebSocket (AC.EffectFnAff (Object (Response a)))

fetchWS :: forall a. WebSocket -> AC.EffectFnAff (Object (Response a))
fetchWS = runFn2 _fetchWS withError