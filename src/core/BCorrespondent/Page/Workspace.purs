module BCorrespondent.Page.Workspace (Output (..), slot) where

import Prelude

import BCorrespondent.Component.Workspace.User as Workspace.User
import BCorrespondent.Component.Workspace.User.Menu as Workspace.User.Menu
import BCorrespondent.Component.HTML.Utils (css, maybeElem, stylishDiv)
import BCorrespondent.Component.Auth.SendResetPassLink as SendResetPassLink
import BCorrespondent.Component.FileUploader as FileUploader 
import BCorrespondent.Component.Workspace.Menu as Workspace.Menu
import BCorrespondent.Component.Workspace.History as Workspace.History
import BCorrespondent.Component.Workspace.Dashboard as Workspace.Dashboard
import BCorrespondent.Component.Workspace.Wallet as Workspace.Wallet
import BCorrespondent.Component.Workspace.TechSupport as Workspace.TechSupport
import BCorrespondent.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..), isJust, fromMaybe)

import Undefined

proxy = Proxy :: _ "workspace"

loc = "BCorrespondent.Page.Workspace"

slot n = HH.slot proxy n component unit

data Acion =
        HandleChildWorkspaceUser Workspace.User.Menu.Output
      | HandleChildWorkspace Workspace.User.Output
      | HandleChildWorkspaceMenu Workspace.Menu.Output


data Output = SignOutForward 

data Component = Dashboard | History | Wallet | TechSupport

type State = { component :: Component }

component =
  H.mkComponent
    { initialState: const { component: Dashboard }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction 
        (HandleChildWorkspaceUser 
          Workspace.User.Menu.LoggedOut) = 
        H.raise SignOutForward
      handleAction 
        (HandleChildWorkspace 
         Workspace.User.DropDownMenu) = 
        H.tell Workspace.User.Menu.proxy 1 $ 
          Workspace.User.Menu.Open
      handleAction
        (HandleChildWorkspace 
         Workspace.User.Notification) =
        let msg = "notifications are to be implemented in the next release" 
        in Async.send $ Async.mkOrdinary msg Async.Debug Nothing
      handleAction (HandleChildWorkspaceMenu out) =
        H.modify_ _ { 
          component = 
          case out of
            Workspace.Menu.Dashboard -> Dashboard
            Workspace.Menu.History -> History
            Workspace.Menu.Wallet -> Wallet
            Workspace.Menu.TechSupport -> TechSupport
        }

render { component } = 
  HH.div_
  [ 
      HH.div [css "user-menu"]
      [ stylishDiv [css "black-square"],
        Workspace.User.slot 0 HandleChildWorkspace, 
        Workspace.User.Menu.slot 1 HandleChildWorkspaceUser 
      ]
  ,   Workspace.Menu.slot 2 HandleChildWorkspaceMenu
  ,   HH.div [css "workspace-body"] [ HH.div [css "centre-container"] [chooseComponent component 3] ]
  ]

chooseComponent Dashboard = Workspace.Dashboard.slot
chooseComponent History = Workspace.History.slot
chooseComponent Wallet = Workspace.Wallet.slot
chooseComponent TechSupport = Workspace.TechSupport.slot
