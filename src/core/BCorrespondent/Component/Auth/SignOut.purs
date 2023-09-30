module BCorrespondent.Component.Auth.SignOut (component, proxy, slot) where

import Prelude

import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (withError)
import BCorrespondent.Data.Dashboard.Output (Output (LoggedOut))

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.Extended as HPExt
import Web.Event.Event (preventDefault, Event)
import Halogen.Store.Monad (getStore, updateStore)
import Data.Traversable (for_)
import Data.Maybe (Maybe (..))
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (removeItem)
import Web.HTML (window)
import Store (Action(UpdateJwtUser))

import Undefined

proxy = Proxy :: _ "auth_signOut"

loc = "BCorrespondent.Component.Auth.SignOut"

slot n = HH.slot proxy n component unit

data Action = MakeLogoutRequest Event

type State = { user :: String }

component =
  H.mkComponent
    { initialState: const { user: mempty :: String }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (MakeLogoutRequest ev) = do 
        H.liftEffect $ preventDefault ev
        { config: Config { apiBCorrespondentHost: host }, user, jwtName } <- getStore
        for_ user \{ token } -> do
          resp <- Request.makeAuth (Just token) host Back.mkAuthApi $ Back.logout
          withError resp \{ success: _ :: Unit } -> do
            H.liftEffect $ 
              window >>= 
                localStorage >>= 
                  removeItem 
                    jwtName
          updateStore $ UpdateJwtUser Nothing
          H.raise LoggedOut

render _ = HH.form [ HE.onSubmit MakeLogoutRequest ] [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "logout" ] ]