module BCorrespondent.Page.Dashboard (Output (..), component, proxy, slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.Auth.SignOut as SignOut

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

import Undefined

proxy = Proxy :: _ "home"

loc = "BCorrespondent.Page.Dashboard"

slot n = HH.slot proxy n component unit

data Output = Logout

data Action = HandleSignOut SignOut.Output

component =
  H.mkComponent
    { initialState: identity
    , render:const render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (HandleSignOut SignOut.LoggedOutSuccess) = H.raise Logout

render = HH.div [ css "loading-container" ] [ HH.text "dashboard", SignOut.slot 0 HandleSignOut ]