-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module BCorrespondent.Component.Root
  ( Action(..)
  , ChildSlots
  , Query(..)
  , State
  , component
  ) where

import Prelude

import BCorrespondent.Component.Utils (OpaqueSlot)
import BCorrespondent.Data.Route (Route(..), routeCodec)
import BCorrespondent.Page.Home as Home
import BCorrespondent.Component.Auth.SignIn as Auth.SignIn
import BCorrespondent.Page.Error.Page500 as Page500
import BCorrespondent.Page.Error.Page404 as Page404
import BCorrespondent.Capability.Navigate
import BCorrespondent.Capability.LogMessages (class LogMessages, logDebug)
import BCorrespondent.Capability.Now (class Now)
import BCorrespondent.Component.Async (Level(..), mkOrdinary, send, mkException) as Async
import BCorrespondent.Component.Root.Fork.Telegram as Fork.Telegram
import BCorrespondent.Component.HTML.Loader as HTML.Loader
import BCorrespondent.Data.Config
import BCorrespondent.Data.Route as Route
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)

import Data.Either (hush, Either(..))
import Data.Foldable (elem, for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Halogen.HTML.Properties as HP
import Store (printStore)
import Effect.AVar as Async
import Data.List (head)
import Data.Map as Map
import Routing.Duplex.Parser (RouteError(EndOfPath))
import AppM (AppM)

import Undefined

loc = " BCorrespondent.Component.Root"

data Query a = Navigate Route a

type State = { route :: Maybe Route }

data Action = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , auth_signIn :: OpaqueSlot Unit
  , error500 :: OpaqueSlot Unit
  , error404 :: OpaqueSlot Unit
  )

component :: H.Component Query Unit Void AppM
component = H.mkComponent
  { initialState: const { route: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void AppM Unit
  handleAction Initialize = do
    logDebug $ loc <> " ---> root component init start .."
    store@{ config: Config { apiBCorrespondentHost }, user } <- getStore
    logDebug $ printStore store

    Fork.Telegram.init >>= Fork.Telegram.fork

    -- first we'll get the route the user landed on
    from <- (RD.parse routeCodec) <$> liftEffect getHash
    -- then we'll navigate to the new route (also setting the hash)
    logDebug $ loc <> " ---> root component init is about to be completed .."
    case from of
      Right route -> navigate route
      Left EndOfPath -> navigate Home
      Left _ -> navigate Error404

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void AppM (Maybe a)
  handleQuery (Navigate dest a) = do
    logDebug $ loc <> " ---> routing to " <> show dest
    store <- getStore
    logDebug $ printStore store
    H.modify_ _ { route = pure dest }
    pure $ Just a

render :: State -> H.ComponentHTML Action ChildSlots AppM
render { route: Nothing } = HTML.Loader.html
render { route: Just r@Home } = HH.slot_ Home.proxy unit Home.component unit
render { route: Just r@SignIn } = HH.slot_ Auth.SignIn.proxy unit  Auth.SignIn.component unit
render { route: Just Error500 } = HH.slot_ Page500.proxy unit Page500.component unit
render { route: Just Error404 } = HH.slot_ Page404.proxy unit Page404.component unit