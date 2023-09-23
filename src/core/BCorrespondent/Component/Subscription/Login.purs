module BCorrespondent.Component.Subscription.Login (subscribe) where

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
    { isLoginVar } <- getStore
    isLogin <- H.liftEffect $ Async.tryTake isLoginVar
    for_ isLogin $ const goCompHandle