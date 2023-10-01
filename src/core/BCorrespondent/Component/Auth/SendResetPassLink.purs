module BCorrespondent.Component.Auth.SendResetPassLink (proxy, slot, component) where

import Prelude

import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (withError)
import BCorrespondent.Data.Dashboard.Output as  Dashboard

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Halogen.Store.Monad (getStore)
import Type.Proxy (Proxy(..))
import Data.Traversable (for_)
import Data.Maybe (Maybe (..))
import AppM (AppM)
import Foreign (Foreign, isNull, unsafeFromForeign)
import Effect.Exception (Error)
import Store (User)

import Undefined

proxy = Proxy :: _ "auth_reset_password"

loc = "BCorrespondent.Component.Auth.SendResetPassLink"

slot n = HH.slot proxy n component unit

data Action = MakeResetLinkRequest Event

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where 
      handleAction (MakeResetLinkRequest ev) = do 
        H.liftEffect $ preventDefault ev
        { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        for_ (user :: Maybe User) \{token} -> do
          resp <- Request.makeAuth (Just token) host Back.mkAuthApi Back.resetPasswordLink
          withError resp \{success: (o :: Foreign)} -> do 
            if isNull o
            then H.raise $ Dashboard.ResetPasswordOk
            else H.raise $ Dashboard.ResetPasswordTimeLeft $ unsafeFromForeign o

render = HH.form [ HE.onSubmit MakeResetLinkRequest ] [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "reset link" ] ]