module BCorrespondent.Component.Subscription.WS (subscribe, module WS) where

import Prelude

import BCorrespondent.Data.Config
import BCorrespondent.Api.Foreign.Request (makeWS)
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Async as Async
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Component.Subscription.WS.Types as WS

import Halogen as H
import Web.Socket as WS
import Web.Socket.ReadyState
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Halogen.Store.Monad (getStore)
import Data.Traversable (for_)
import Effect.AVar as Async
import Data.Array ((:), singleton)
import Data.Argonaut.Encode (encodeJson)


subscribe loc url trigger goCompHandle = do
  { config: Config { apiBCorrespondentHostWS }, user, wsVar } <- getStore
  logDebug $ loc <> " ---> ws url: " <> apiBCorrespondentHostWS <> "/" <> url
  ws <- H.liftEffect $ WS.create (apiBCorrespondentHostWS <> "/" <> url) []
  let
    isOpen = do
      Aff.delay $ Aff.Milliseconds 100.0
      st <- H.liftEffect $ WS.readState ws
      if st == Open then pure true else isOpen
  H.liftAff $ unlessM isOpen $ pure unit

  -- esteblish authorised connection
  -- if it is successful we'll prcoceed with component handler
  for_ user \{ token } -> do
    H.liftEffect $ ws `WS.send` token
    resp <- makeWS ws
    onFailure resp (Async.send <<< flip Async.mkException loc) $ \_ -> do
      logDebug $ loc <> " ---> ws is open "
      for_ trigger \init -> H.liftEffect $ ws `WS.send` encodeJson init
      forkId <- H.fork $ forever $ do
        resp <- makeWS ws
        onFailure resp (Async.send <<< flip Async.mkException loc) goCompHandle
      void $ H.liftEffect $ do
        st <- Async.status wsVar
        if Async.isEmpty st then void $ singleton { ws: ws, forkId: forkId } `Async.tryPut` wsVar
        else do
          mxs <- Async.tryTake wsVar
          for_ mxs \xs -> ({ ws: ws, forkId: forkId } : xs) `Async.tryPut` wsVar