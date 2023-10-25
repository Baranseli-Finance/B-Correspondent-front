module BCorrespondent.Page.Workspace (Output (..), slot) where

import Prelude

import BCorrespondent.Component.Workspace.User as Workspace.User
import BCorrespondent.Component.Workspace.User.Menu as Workspace.User.Menu
import BCorrespondent.Component.HTML.Utils (css, maybeElem, stylishDiv)
import BCorrespondent.Component.Auth.SendResetPassLink as SendResetPassLink
import BCorrespondent.Component.FileUploader as FileUploader 
import BCorrespondent.Component.Workspace.Menu as Workspace.Menu
import BCorrespondent.Component.Workspace.Dashboard as Workspace.Dashboard
import BCorrespondent.Component.Workspace.Wallet as Workspace.Wallet
import BCorrespondent.Component.Workspace.TechSupport as Workspace.TechSupport
import BCorrespondent.Component.Workspace.User.Notification as User.Notification
import BCorrespondent.Component.Workspace.BalancedBook as Workspace.BalancedBook
import BCorrespondent.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..), isJust, fromMaybe)
import Halogen.Store.Monad (getStore)
import Data.Date as D
import Data.Enum (fromEnum)

import Undefined

proxy = Proxy :: _ "workspace"

loc = "BCorrespondent.Page.Workspace"

slot n = HH.slot proxy n component unit

data Acion =
        HandleChildWorkspaceUser Workspace.User.Menu.Output
      | HandleChildWorkspace Workspace.User.Output
      | HandleChildWorkspaceMenu Workspace.Menu.Output
      | HandleChildBalancedBook Workspace.BalancedBook.Output


data Output = SignOutForward 

data Component = 
       Dashboard (Maybe Int)
     | BalancedBook
     | Wallet
     | TechSupport

type State = { component :: Component }

component =
  H.mkComponent
    { initialState: const { component: Dashboard Nothing }
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
        H.tell User.Notification.proxy 2 $ User.Notification.Open
      handleAction 
        (HandleChildBalancedBook
         (Workspace.BalancedBook.OutputLive from)) = do 
           H.modify_ _ { component = Dashboard Nothing }
           H.tell Workspace.Menu.proxy 3 $ 
             Workspace.Menu.SwithToMenu 
               (fromEnum Workspace.Menu.OpenDashboard)
      handleAction (HandleChildWorkspaceMenu out) =
        H.modify_ _ {
          component = 
          case out of
            Workspace.Menu.Dashboard -> Dashboard Nothing
            Workspace.Menu.BalancedBook -> BalancedBook
            Workspace.Menu.Wallet -> Wallet
            Workspace.Menu.TechSupport -> TechSupport
        }

render { component } = 
  HH.div_
  [ 
      HH.div [css "user-menu"]
      [ stylishDiv [css "black-square"],
        Workspace.User.slot 0 HandleChildWorkspace, 
        Workspace.User.Menu.slot 1 HandleChildWorkspaceUser,
        User.Notification.slot 2

      ]
  ,   Workspace.Menu.slot 3 HandleChildWorkspaceMenu
  ,   HH.div
      [css "workspace-body"]
      [ HH.div
        [css "centre-container"]
        [chooseComponent component 4] 
      ]
  ]

chooseComponent (Dashboard _) = Workspace.Dashboard.slot
chooseComponent BalancedBook = flip Workspace.BalancedBook.slot HandleChildBalancedBook
chooseComponent Wallet = Workspace.Wallet.slot
chooseComponent TechSupport = Workspace.TechSupport.slot
