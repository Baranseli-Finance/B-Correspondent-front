module BCorrespondent.Page.Home
  ( Action(..)
  , component
  , proxy
  ) where

import Prelude

import BCorrespondent.Page.Subscription.WinResize as WinResize
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Data.Route as Route
import BCorrespondent.Data.Config
import BCorrespondent.Component.HTML.Utils (css, safeHref, whenElem)
import BCorrespondent.Page.Dashboard as Dashboard 
import BCorrespondent.Component.Auth.SignIn as SignIn
import BCorrespondent.Component.Subscription.Login as Subscription.Login

import Halogen.HTML.Properties.Extended as HPExt
import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Action(WriteError))
import Store.Types (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import Data.Map as Map
import System.Time (getTimestamp)

import Undefined

proxy = Proxy :: _ "home"

loc = "BCorrespondent.Page.Home"

data Action
  = Initialize
  | WinResize Int
  | Login

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  , isUser :: Boolean
  }

component =
  H.mkComponent
    { initialState: const
        { winWidth: Nothing
        , platform: Nothing
        , isUser: false
        }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  render { winWidth: Just _, platform: Just _, isUser: true } = 
    HH.div_ [ HH.slot_ Dashboard.proxy unit Dashboard.component unit ]
  render { winWidth: Just _, platform: Just _, isUser: false } = 
    HH.div_ [ HH.slot_ SignIn.proxy unit SignIn.component unit ]
  render _ = HH.div_ []
  handleAction Initialize = do
    H.liftEffect $ window >>= document >>= setTitle "BCorrespondent | Home"
    { platform, user } <- getStore
    w <- H.liftEffect $ window >>= innerWidth

    H.modify_ _ { platform = pure platform, winWidth = pure w, isUser = isJust user }

    void $ H.subscribe =<< WinResize.subscribe WinResize

    Subscription.Login.subscribe loc $ handleAction Login

  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
  handleAction Login = H.modify_ _ { isUser = true }