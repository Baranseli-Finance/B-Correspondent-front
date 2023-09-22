module Buzgibi.Api.Foreign.Request
  ( make
  , makeAuth
  , makeAuthWithResp
  , makeWS
  , makeWithResp
  ) where

import Prelude

import Buzgibi.Api.Foreign.BuzgibiBack

import Effect
import Data.Function.Uncurried (runFn1, runFn2)
import Effect.Exception (Error, error)
import Effect.Aff.Compat as AC
import Halogen as H
import Data.Either
import Effect.Aff (try)
import Effect.Aff.Class
import Foreign.Object (Object)
import Data.Traversable (for)
import Data.Nullable
import Data.Bifunctor (rmap)
import Data.Maybe
import Safe.Coerce
import Web.Socket as WS

makeAuthWithResp
  :: forall m api resp
   . MonadAff m
  => Maybe JWTToken
  -> String
  -> (ApiClient -> Effect api)
  -> (api -> AC.EffectFnAff (Object (Response resp)))
  -> m (Either Error (Object (Response resp)))
makeAuthWithResp token host mkApi runApi = do
  let jwt = map coerce token
  api <- H.liftEffect $ do mkApiClient jwt host >>= mkApi
  H.liftAff $ try $ AC.fromEffectFnAff $ runApi api

makeWithResp
  :: forall m api resp
   . MonadAff m
  => String
  -> (ApiClient -> Effect api)
  -> (api -> AC.EffectFnAff (Object (Response resp)))
  -> m (Either Error (Object (Response resp)))
makeWithResp = makeAuthWithResp Nothing

makeAuth
  :: forall m api resp a
   . MonadAff m
  => Maybe JWTToken
  -> String
  -> (ApiClient -> Effect api)
  -> (api -> AC.EffectFnAff (Object resp))
  -> m (Either Error (Success a))
makeAuth token host mkApi runApi = do
  let jwt = map coerce token
  api <- H.liftEffect $ do mkApiClient jwt host >>= mkApi
  obj <- H.liftAff $ try $ AC.fromEffectFnAff $ runApi api
  val <- map join $ for obj (H.liftEffect <<< getDataFromObj)
  let msg = "wrong type has been recieved: `{success: null}``. `success` must always be populated with either value or error"
  pure $ val -- <#> \(x :: Success a) -> maybe (Left (error msg)) Right $ toMaybe x

make
  :: forall m api resp a
   . MonadAff m
  => String
  -> (ApiClient -> Effect api)
  -> (api -> AC.EffectFnAff (Object resp))
  -> m (Either Error (Success a))
make = makeAuth Nothing

makeWS :: forall m a. MonadAff m => WS.WebSocket -> m (Either Error (Success a))
makeWS ws = do
  obj <- H.liftAff $ try $ AC.fromEffectFnAff $ fetchWS ws
  map join $ for obj (H.liftEffect <<< getDataFromObjWS)