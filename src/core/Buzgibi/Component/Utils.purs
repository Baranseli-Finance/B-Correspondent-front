-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module Buzgibi.Component.Utils (OpaqueSlot, withCaptcha, initTranslation, with404, withAuth) where

import Prelude

import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Data.Config
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route

import Halogen as H
import Data.Function.Uncurried (runFn2)
import Halogen.Store.Monad (getStore)
import Effect.Exception (throwException, error)
import Effect.Aff as Aff
import Effect.AVar as Async
import Data.Traversable (for)
import Data.Maybe (Maybe(..), isNothing)
import Cache (readTranslation)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot = forall query. H.Slot query Void slot

withCaptcha true onFailure onSuccess = do
  { config: Config { apiBuzgibiHost: host } } <- getStore
  resp <- Request.make host BuzgibiBack.mkReCaptchaApi $
    BuzgibiBack.goReCaptcha "6Ld138ImAAAAAEB8Ba7V5QTvfFhq433MsF5hZV4v"
  logDebug $ "captcha resp --> " <> show resp
  withError resp \{ success: captcha :: BuzgibiBack.ReCaptcha } -> do
    let res = BuzgibiBack.getSuccessReCaptcha captcha
    case res of
      true -> onSuccess
      false -> onFailure
withCaptcha false _ onSuccess = onSuccess

initTranslation loc goCompHandle = do
  let
    try c | c == 5 =
      H.liftEffect
        $ throwException
        $
          error "initialization attempts have been exhausted"
    try c = do
      H.liftAff $ Aff.delay $ Aff.Milliseconds 100.0
      logDebug $ loc <> " ---> initialization attempt: " <> show c
      { cache } <- getStore
      { hash: compHash } <- H.get
      let res = readTranslation cache
      case res of
        Just { value, hash: cacheHash } -> do
          if compHash /= cacheHash then do
            logDebug $ loc <> " ---> tranlation has been initialized"
            goCompHandle cacheHash value
            pure $ Just unit
          else pure Nothing
        Nothing -> try $ c + 1
  try 0

with404 (Just x) go = go x
with404 Nothing _ = navigate Route.Error404

withAuth (Just x) go = go x
withAuth Nothing _ = navigate Route.Home
