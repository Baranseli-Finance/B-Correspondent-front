module Buzgibi.Component.Subscription.Translation (subscribe) where

import Prelude

import Halogen as H
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Effect.AVar as Async
import Halogen.Store.Monad (getStore)
import Data.Traversable (for_)
import Cache (readTranslation)
import Data.Maybe (Maybe(..))

subscribe loc goCompHandle =
  void $ H.fork $ forever $ do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
    { cache } <- getStore
    { hash: compHash } <- H.get
    let res = readTranslation cache
    case res of
      Just { value, hash: cacheHash } ->
        when (compHash /= cacheHash) $
          goCompHandle cacheHash value
      Nothing -> pure unit