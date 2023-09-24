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
import BCorrespondent.Component.Async as Async

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
  | HandleSignIn SignIn.Output

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
    HH.div_ [ HH.slot_ Async.proxy 0 Async.component unit, 
              HH.slot_ Dashboard.proxy 1 Dashboard.component unit
            ]
  render { winWidth: Just _, platform: Just _, isUser: false } = 
    HH.div [ css "centre-container" ] [ HH.slot SignIn.proxy 0 SignIn.component unit HandleSignIn ]
  render _ = HH.div_ []
  handleAction Initialize = do
    H.liftEffect $ window >>= document >>= setTitle "BCorrespondent | Home"
    { platform, user } <- getStore
    w <- H.liftEffect $ window >>= innerWidth

    H.modify_ _ { platform = pure platform, winWidth = pure w, isUser = isJust user }

    void $ H.subscribe =<< WinResize.subscribe WinResize

  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
  handleAction (HandleSignIn (SignIn.LoggedInSuccess {email})) =
    do H.modify_ _ { isUser = true }
       let welcome = "Welcome to the service, " <> email
       Async.send $ Async.mkOrdinary welcome Async.Success Nothing