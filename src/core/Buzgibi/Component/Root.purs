-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module Buzgibi.Component.Root
  ( Action(..)
  , ChildSlots
  , Query(..)
  , State
  , component
  ) where

import Prelude

import Buzgibi.Component.Utils (OpaqueSlot)
import Buzgibi.Data.Route (Route(..), routeCodec)
import Buzgibi.Page.Home as Home
import Buzgibi.Page.Error.Page500 as Page500
import Buzgibi.Page.Error.Page404 as Page404
import Buzgibi.Capability.Navigate
import Buzgibi.Capability.LogMessages (class LogMessages, logDebug)
import Buzgibi.Capability.Now (class Now)
import Buzgibi.Component.HTML.Header as Header
import Buzgibi.Component.HTML.Footer as Footer
import Buzgibi.Component.HTML.Body as Body
import Buzgibi.Component.Lang.Data (Lang(..))
import Buzgibi.Component.Async (Level(..), mkOrdinary, send, mkException) as Async
import Buzgibi.Component.Root.Fork.Translation as Fork.Translation
import Buzgibi.Component.Root.Fork.Telegram as Fork.Telegram
import Buzgibi.Component.HTML.Loading as HTML.Loading
import Buzgibi.Data.Config
import Buzgibi.Component.Auth.SignUp as SignUp
import Buzgibi.Component.Auth.SignIn as SignIn
import Buzgibi.Component.Auth.Email as Auth.Email
import Buzgibi.Page.Auth as Auth
import Buzgibi.Data.Route as Route
import Buzgibi.Page.User.Survey as User.Survey
import Buzgibi.Page.User.History as User.History
import Buzgibi.Component.Survey.Edit as Survey.Edit
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.Request.Handler (onFailure)
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack

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

loc = " Buzgibi.Component.Root"

data Query a = Navigate Route a

type State = { route :: Maybe Route, lang :: Lang }

data Action = Initialize | LangChange Lang

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , error500 :: OpaqueSlot Unit
  , error404 :: OpaqueSlot Unit
  , auth_container_sign_in :: OpaqueSlot Unit
  , auth_container_sign_up :: OpaqueSlot Unit
  , user_survey :: OpaqueSlot Unit
  , user_history :: OpaqueSlot Unit
  , survey_edit :: OpaqueSlot Unit
  , auth_email_confirmation :: OpaqueSlot Unit
  )

component :: H.Component Query Unit Void AppM
component = H.mkComponent
  { initialState: const { route: Nothing, lang: Eng }
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
    store@{ config: Config { isCaptcha, apiBuzgibiHost }, user } <- getStore
    logDebug $ printStore store

    for_ user \{ token, jwtUser: { ident } } -> do
      resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi BuzgibiBack.getNotification
      onFailure resp (Async.send <<< flip Async.mkException loc) $
        \{ success: xs :: Array BuzgibiBack.Notification } ->
          for_ xs \{ text } -> Async.send $ Async.mkOrdinary text Async.Info Nothing

    -- show up info if captcha is disabled
    when (not isCaptcha) $ Async.send $ Async.mkOrdinary "captcha is disabled" Async.Info Nothing

    Fork.Telegram.init >>= Fork.Telegram.fork

    Fork.Translation.init
    Fork.Translation.fork $ handleAction <<< LangChange

    -- first we'll get the route the user landed on
    from <- (RD.parse routeCodec) <$> liftEffect getHash
    -- then we'll navigate to the new route (also setting the hash)
    logDebug $ loc <> " ---> root component init is about to be completed .."
    case from of
      Right route -> navigate route
      Left EndOfPath -> navigate Home
      Left _ -> navigate Error404
  handleAction (LangChange lang) = H.modify_ _ { lang = lang }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void AppM (Maybe a)
  handleQuery (Navigate dest a) = do
    logDebug $ loc <> " ---> routing to " <> show dest
    store <- getStore
    logDebug $ printStore store
    H.modify_ _ { route = pure dest }
    pure $ Just a

params =
  { header: Header.html
  , footer: Footer.html
  }

render :: State -> H.ComponentHTML Action ChildSlots AppM
render { route: Nothing } = HTML.Loading.html
render { route: Just r@Home } = HH.slot_ Home.proxy unit (Home.component (Body.mkBodyHtml params r)) unit
render { route: Just r@SignIn } =
  HH.slot_ Auth.proxy_sign_in unit
    (Auth.component (Body.mkBodyHtml params r) SignIn.slot)
    { route: Route.SignIn, title: "SignIn" }
render { route: Just r@SignUp } =
  HH.slot_ Auth.proxy_sign_up unit
    (Auth.component (Body.mkBodyHtml params r) SignUp.slot)
    { route: Route.SignUp, title: "SignUp" }
render { route: Just r@UserSurvey } = HH.slot_ User.Survey.proxy unit (User.Survey.component (Body.mkBodyHtml params r)) unit
render { route: Just r@(UserHistory page) } = HH.slot_ User.History.proxy unit (User.History.component (Body.mkBodyHtml params r)) { page: page }
render { route: Just Error500 } = HH.slot_ Page500.proxy unit Page500.component unit
render { route: Just Error404 } = HH.slot_ Page404.proxy unit Page404.component unit
render { route: Just r@(EditSurvey _) } = HH.slot_ Survey.Edit.proxy unit (Survey.Edit.component (Body.mkBodyHtml params r)) unit
render { route: Just r@(EmailConfirmation key) } = HH.slot_ Auth.Email.proxy unit Auth.Email.component { key: key }