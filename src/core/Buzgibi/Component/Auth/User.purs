module Buzgibi.Component.Auth.User
  ( component
  , proxy
  ) where

import Prelude

import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Data.Config (Config(..))
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Data.Route (Route(..), defUserHistoryParam)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, Event)
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Halogen.Store.Monad (getStore, updateStore)
import Data.Foldable (for_)
import Store (Action(UpdateJwtUser))
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (removeItem)
import Web.HTML (window)
import Effect.AVar (tryPut, tryTake) as Async
import Effect.Aff as Aff
import Data.String.CodeUnits (takeWhile)

import Undefined

loc = "Buzgibi.Component.Auth.User"

proxy = Proxy :: _ "auth_user"

type State = { email :: Maybe String }

data Action = Initialize | MakeRequest Event | Finalize

component =
  H.mkComponent
    { initialState: const { email: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , finalize = pure Finalize
        }
    }
  where
  handleAction Initialize = do
    { user } <- getStore
    for_ user \{ jwtUser: { email } } -> H.modify_ _ { email = Just email }
  handleAction (MakeRequest ev) = do
    H.liftEffect $ preventDefault ev
    { config: Config { apiBuzgibiHost }, user, isLogoutVar } <- getStore
    case user of
      Just { token } -> do
        resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.logout
        withError resp \{ success: _ :: Unit } -> do
          H.liftEffect $ window >>= localStorage >>= removeItem "buzgibi_jwt"
          H.modify_ _ { email = Nothing }
          updateStore $ UpdateJwtUser Nothing
          res <- H.liftEffect $ Async.tryPut unit isLogoutVar
          logDebug $ loc <> " if avar operaion succeeded ---> " <> show res
      Nothing -> pure unit
  handleAction Finalize = do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
    { isLogoutVar } <- getStore
    void $ H.liftEffect $ Async.tryTake isLogoutVar
    logDebug $ loc <> " ----> logout var has been emptied"

render { email: Just email } =
  HH.form
    [ HE.onSubmit MakeRequest
    , css "form-inline"
    ]
    [ HH.a
        [ css "nav-link"
        , safeHref (UserHistory defUserHistoryParam)
        ]
        [ HH.text $ takeWhile ((/=) '@') email ]
    , HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "Sign out" ]
    ]
render { email: Nothing } = HH.div_ []