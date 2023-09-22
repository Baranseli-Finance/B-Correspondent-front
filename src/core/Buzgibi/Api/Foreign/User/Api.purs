module Buzgibi.Api.Foreign.User.Api
  ( History
  , Location
  , Notification
  , SubmitSurvey
  , Survey
  , UserApi
  , WithFieldStatusHistoryItem
  , editSurvey
  , getHistory
  , getNotification
  , makeSurvey
  , mkUserApi
  , printWithFieldStatusHistoryItem
  , submitSurvey
  ) where

import Prelude

import Buzgibi.Api.Foreign.Common

import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn3, Fn4, runFn3, runFn4, Fn2, runFn2)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign, typeOf)
import Data.Either (Either)
import Effect.Exception as E
import Data.Maybe (Maybe, fromMaybe)

foreign import data UserApi :: Type

foreign import mkUserApi :: Fn1 ApiClient (Effect UserApi)

type Location = { latitude :: Number, longitude :: Number }

type Survey =
  { survey :: String
  , assessmentscore :: String
  , category :: String
  , phonesfileident :: Int
  , location :: Location
  }

foreign import _makeSurvey :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Survey UserApi (AC.EffectFnAff (Object (Response Unit)))

makeSurvey :: Survey -> UserApi -> AC.EffectFnAff (Object (Response Unit))
makeSurvey = runFn3 _makeSurvey withError

type SubmitSurvey = { ident :: Int }

foreign import _submitSurvey :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) SubmitSurvey UserApi (AC.EffectFnAff (Object (Response Boolean)))

submitSurvey :: SubmitSurvey -> UserApi -> AC.EffectFnAff (Object (Response Boolean))
submitSurvey = runFn3 _submitSurvey withError

type EditSurvey = { survey :: String }

foreign import _editSurvey :: Fn4 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Int EditSurvey UserApi (AC.EffectFnAff (Object (Response Boolean)))

editSurvey :: Int -> EditSurvey -> UserApi -> AC.EffectFnAff (Object (Response Boolean))
editSurvey = runFn4 _editSurvey withError

type WithFieldStatusHistoryItem =
  { surveyident :: Int
  , reportident :: Foreign
  , status :: String
  , name :: String
  , timestamp :: String
  , voice :: Foreign
  }

printWithFieldStatusHistoryItem { surveyident, reportident, status, name, timestamp, voice } =
  "{ surveyident: " <> show surveyident
    <> "reportident:"
    <> typeOf reportident
    <> ", status:"
    <> status
    <> ", name: "
    <> name
    <> ", tm: "
    <> timestamp
    <>
      ", voice: <bytes> }"

type History = { items :: Array WithFieldStatusHistoryItem, total :: Int, perpage :: Int }

type Page = { page :: Int }

foreign import _getHistory :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Page UserApi (AC.EffectFnAff (Object (Response History)))

getHistory :: Maybe Page -> UserApi -> AC.EffectFnAff (Object (Response History))
getHistory page = runFn3 _getHistory withError (fromMaybe { page: 1 } page)

type Notification = { text :: String, level :: String }

foreign import _getNotification :: Fn2 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) UserApi (AC.EffectFnAff (Object (Response (Array Notification))))

getNotification :: UserApi -> AC.EffectFnAff (Object (Response (Array Notification)))
getNotification = runFn2 _getNotification withError
