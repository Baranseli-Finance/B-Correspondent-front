-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module BCorrespondent.Component.Utils (OpaqueSlot, with404, withAuth, withLoader) where

import Prelude

import BCorrespondent.Capability.Navigate (navigate)
import BCorrespondent.Data.Route as Route

import Halogen as H
import Data.Function.Uncurried (runFn2)
import Halogen.Store.Monad (getStore)
import Effect.Exception (throwException, error)
import Effect.Aff as Aff
import Effect.AVar as Async
import Data.Traversable (for)
import Data.Maybe (Maybe(..), isNothing)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot = forall query. H.Slot query Void slot

with404 (Just x) go = go x
with404 Nothing _ = navigate Route.Error404

withAuth (Just x) go = go x
withAuth Nothing _ = navigate Route.Home

withLoader fn = H.modify_ _ { isLoading = true } *> fn *> H.modify_ _ { isLoading = false }