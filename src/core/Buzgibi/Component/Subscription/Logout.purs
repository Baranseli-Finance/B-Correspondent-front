module Buzgibi.Component.Subscription.Logout (subscribe) where

import Prelude

import Halogen as H
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Effect.AVar as Async
import Halogen.Store.Monad (getStore)
import Data.Traversable (for_)
import Data.Maybe (Maybe(..))

subscribe loc goCompHandle =
  void $ H.fork $ forever $ do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
    { isLogoutVar } <- getStore
    isLogout <- H.liftEffect $ Async.tryRead isLogoutVar
    for_ isLogout $ const goCompHandle