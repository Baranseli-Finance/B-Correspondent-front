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

data Component = Dashboard | History

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
         Workspace.User.OpenDropDownMenu) = 
        H.tell Workspace.User.Menu.proxy 1 $ 
          Workspace.User.Menu.Open
      handleAction (HandleChildWorkspaceMenu out) =
        H.modify_ _ { 
          component = 
          case out of
            Workspace.Menu.Dashboard -> Dashboard
            Workspace.Menu.History -> History
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
  ,   HH.div [css "workspace-body"] [ HH.div [css "centre-container"] [choseComponent component] ]
  ]

choseComponent Dashboard = Workspace.Dashboard.slot 3
choseComponent History = Workspace.History.slot 3