module BCorrespondent.Page.Home
  ( Action(..)
  , component
  , proxy
  , slot
  ) where

import Prelude

import BCorrespondent.Page.Subscription.WinResize as WinResize
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Data.Route as Route
import BCorrespondent.Data.Config
import BCorrespondent.Component.HTML.Utils (css, safeHref, chooseElem)
import BCorrespondent.Page.Workspace as Workspace 
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
import AppM (AppM)

import Undefined

proxy = Proxy :: _ "home"

loc = "BCorrespondent.Page.Home"

slot = HH.slot_ proxy unit component unit

data Action
  = Initialize
  | WinResize Int
  | HandleChildWorkspace Workspace.Output
  | HandleChildSignIn SignIn.Output

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
  handleAction Initialize = do
    { platform, user } <- getStore
    H.liftEffect $ 
      window >>= 
        document >>= 
          setTitle
            (if isJust user 
            then "BCorrespondent | Workspace"
            else "BCorrespondent | Home")
    w <- H.liftEffect $ window >>= innerWidth
    H.modify_ _ 
      { platform = pure platform, 
        winWidth = pure w, 
        isUser = isJust user 
      }
    void $ H.subscribe =<< WinResize.subscribe WinResize
  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
  handleAction (HandleChildSignIn (SignIn.LoggedInSuccess {login})) =
    do H.modify_ _ { isUser = true }
       H.liftEffect $ 
         window >>= 
           document >>= 
             setTitle 
             "Correspondent | Workspace"
       let welcome = "Welcome to the service, " <> login
       Async.send $ Async.mkOrdinary welcome Async.Success Nothing
  handleAction (HandleChildWorkspace Workspace.SignOutForward) =
    do H.modify_ _ { isUser = false }
       H.liftEffect $ 
         window >>= 
           document >>= 
             setTitle 
             "Correspondent | Home"
       let logout = "you have been logged out"
       Async.send $ Async.mkOrdinary logout Async.Success Nothing
 
render { winWidth: Just _, platform: Just _, isUser } =
  HH.div 
  [HPExt.id "wrapper"]
  [ HH.div [css "alert-container"] [Async.slot 0], 
    HH.div [css "body-container"] [body] 
  ]
  where 
    body = 
      chooseElem 
        isUser
        (Workspace.slot 1 HandleChildWorkspace) $
        HH.div
         [ css "centre-container" ]
         [ welcome, SignIn.slot 1 HandleChildSignIn ]
render _ = HH.div_ []

welcome = HH.div [] [HH.h2_ [HH.text "Welcome to B-Correspondent!"]]