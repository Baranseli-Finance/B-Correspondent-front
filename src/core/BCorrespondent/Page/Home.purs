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
import BCorrespondent.Data.Home.Output as Home.Output

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
import AppM (AppM)

import Undefined

proxy = Proxy :: _ "home"

loc = "BCorrespondent.Page.Home"

data Action
  = Initialize
  | WinResize Int
  | HandleChild Home.Output.Output

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  , isUser :: Boolean
  }

component :: forall q . H.Component q Unit Void AppM
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
    HH.div_ [ Async.slot 0, Dashboard.slot 1 HandleChild ]        
  render { winWidth: Just _, platform: Just _, isUser: false } = 
    HH.div_ [ Async.slot 0, HH.div [ css "centre-container" ] [ SignIn.slot 1 HandleChild ] ]
  render _ = HH.div_ []
  handleAction Initialize = do
    H.liftEffect $ window >>= document >>= setTitle "BCorrespondent | Home"
    { platform, user } <- getStore
    w <- H.liftEffect $ window >>= innerWidth

    H.modify_ _ { platform = pure platform, winWidth = pure w, isUser = isJust user }

    void $ H.subscribe =<< WinResize.subscribe WinResize

  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
  handleAction (HandleChild (Home.Output.LoggedInSuccess {login})) =
    do H.modify_ _ { isUser = true }
       let welcome = "Welcome to the service, " <> login
       Async.send $ Async.mkOrdinary welcome Async.Success Nothing
  handleAction (HandleChild Home.Output.LoggedOutSuccess) =
    do H.modify_ _ { isUser = false }
       let logout = "you have been logged out"
       Async.send $ Async.mkOrdinary logout Async.Success Nothing