module Buzgibi.Component.Subscription.WS (subscribe) where

import Prelude

import Buzgibi.Data.Config
import Buzgibi.Api.Foreign.Request (makeWS)
import Buzgibi.Api.Foreign.Request.Handler (onFailure)
import Buzgibi.Component.Async as Async
import Buzgibi.Capability.LogMessages (logDebug)

import Halogen as H
import Web.Socket as WS
import Web.Socket.ReadyState
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Halogen.Store.Monad (getStore)
import Data.Traversable (for_)
import Effect.AVar as Async
import Data.Array ((:), singleton)

import Undefined

subscribe loc url trigger goCompHandle = do
  { config: Config { apiBuzgibiHostWS }, user, wsVar } <- getStore
  logDebug $ loc <> " ---> ws url: " <> apiBuzgibiHostWS <> "/" <> url
  ws <- H.liftEffect $ WS.create (apiBuzgibiHostWS <> "/" <> url) []
  let
    isOpen = do
      Aff.delay $ Aff.Milliseconds 1000.0
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
      for_ trigger \init -> H.liftEffect $ ws `WS.send` init
      forkId <- H.fork $ forever $ do
        resp <- makeWS ws
        onFailure resp (Async.send <<< flip Async.mkException loc) goCompHandle
      void $ H.liftEffect $ do
        st <- Async.status wsVar
        if Async.isEmpty st then void $ singleton { ws: ws, forkId: forkId } `Async.tryPut` wsVar
        else do
          mxs <- Async.tryTake wsVar
          for_ mxs \xs -> ({ ws: ws, forkId: forkId } : xs) `Async.tryPut` wsVar