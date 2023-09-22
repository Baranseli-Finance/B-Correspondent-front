module Buzgibi.Component.Root.Fork.Translation (fork, init) where

import Prelude

import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.Request.Handler as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.Async as Async
import Buzgibi.Capability.LogMessages (logDebug, logInfo)
import Buzgibi.Data.Config

import Halogen as H
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Effect.AVar as Async
import Halogen.Store.Monad (getStore, updateStore)
import Data.Traversable (for_)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), isNothing)
import Store (Action(WriteTranslationToCache))
import Crypto.Hash (createHash)

loc = "Buzgibi.Component.Root.Fork.Translation:fork"

translation = Proxy :: _ "translation"

fork goRootHandle =
  void $ H.fork $ forever $ do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
    { langVar, config: Config { apiBuzgibiHost: host } } <- getStore
    res <- H.liftEffect $ Async.tryRead langVar
    { lang: curr } <- H.get
    for_ res \income ->
      when (curr /= income) $ do
        logDebug $ loc <> " ---> " <> show income
        resp <- Request.make host BuzgibiBack.mkFrontApi $ BuzgibiBack.loadTranslation income
        Request.onFailure resp (Async.send <<< flip Async.mkException loc)
          \{ success: translation } -> do
            hash <- H.liftEffect $ createHash translation
            updateStore $ WriteTranslationToCache translation hash
            logDebug $ loc <> " ---> translation cache has been updated, hash: " <> hash
            goRootHandle income

init = do
  { langVar, config: Config { apiBuzgibiHost: host } } <- getStore
  res <- H.liftEffect $ Async.tryRead langVar
  for_ res \lang -> do
    resp <- Request.make host BuzgibiBack.mkFrontApi $ BuzgibiBack.loadTranslation lang
    Request.withError resp $ \{ success: translation } -> do
      hash <- H.liftEffect $ createHash translation
      logDebug $ loc <> " ---> translation cache has been created, hash: " <> hash
      updateStore $ WriteTranslationToCache translation hash