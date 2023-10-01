module BCorrespondent.Page.Dashboard (Output (..), component, proxy, slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.Auth.SignOut as SignOut
import BCorrespondent.Component.Auth.SendResetPassLink as SendResetPassLink
import BCorrespondent.Data.Dashboard.Output as Dashboard
import BCorrespondent.Data.Home.Output as Home

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..), isNothing, fromMaybe)

import Undefined

proxy = Proxy :: _ "dashboard"

loc = "BCorrespondent.Page.Dashboard"

slot n = HH.slot proxy n component unit

data Output = HandleChild Dashboard.Output

type State = { tmleft :: Maybe Int }

component =
  H.mkComponent
    { initialState: const { tmleft: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction 
        (HandleChild 
          Dashboard.LoggedOut) = 
        H.raise Home.LoggedOutSuccess
      handleAction 
        (HandleChild 
          (Dashboard.ResetPasswordTimeLeft tmleft)) = 
        H.modify_ _ { tmleft = Just tmleft }
      handleAction 
        (HandleChild 
          (Dashboard.ResetPasswordOk)) = do
        H.modify_ _ { tmleft = Nothing }
        H.raise Home.PasswordResetLinkSend

render { tmleft } = 
  HH.div 
  [ css "loading-container" ] 
  [ HH.text "dashboard",
    SignOut.slot 0 HandleChild,
    SendResetPassLink.slot 1 HandleChild,
    if isNothing tmleft then HH.div_ []
    else 
      HH.div 
      [ HPExt.style "margin-bottom: 10px;" ] 
      [ HH.span [ HPExt.style "color: red" ] 
        [ HH.text $ 
            "the next attemp to send \ 
            \ a reset link is in " <> 
            show  (fromMaybe undefined tmleft) <> " sec" 
        ]
      ]
  ]