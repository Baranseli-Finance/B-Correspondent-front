module Buzgibi.Page.Subscription.WinResize (subscribe) where

import Prelude

import Halogen as H
import Halogen.Subscription as HS
import Effect.Aff as Aff
import Control.Monad.Rec.Class (forever, class MonadRec)
import Effect.Aff (Milliseconds(..), Aff)
import Effect.Aff.Class (liftAff)
import Web.HTML.Window (innerWidth)
import Web.HTML (window)

subscribe go = do
  { emitter, listener } <- H.liftEffect HS.create
  void $ H.fork $ forever $ do
    liftAff $ Aff.delay $ Milliseconds 500.0
    w <- H.liftEffect $ window >>= innerWidth
    H.liftEffect $ HS.notify listener $ go w
  pure emitter