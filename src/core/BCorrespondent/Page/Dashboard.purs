module BCorrespondent.Page.Dashboard (Output (..), component, proxy, slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.Auth.SignOut as SignOut
import BCorrespondent.Data.Home.Output

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

import Undefined

proxy = Proxy :: _ "dashboard"

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
      handleAction (HandleSignOut SignOut.Logout) = H.raise LoggedOutSuccess

render = HH.div [ css "loading-container" ] [ HH.text "dashboard", SignOut.slot 0 HandleSignOut ]