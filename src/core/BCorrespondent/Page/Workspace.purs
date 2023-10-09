module BCorrespondent.Page.Workspace (Output (..), slot) where

import Prelude

import BCorrespondent.Component.Workspace.User as Workspace.User
import BCorrespondent.Component.Workspace.User.Menu as Workspace.User.Menu
import BCorrespondent.Component.HTML.Utils (css, maybeElem)
import BCorrespondent.Component.Auth.SendResetPassLink as SendResetPassLink
import BCorrespondent.Component.FileUploader as FileUploader 

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


data Output = SignOutForward 

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
        (HandleChildWorkspaceUser 
          Workspace.User.Menu.LoggedOut) = 
        H.raise SignOutForward
      handleAction (HandleChildWorkspace Workspace.User.OpenDropDownMenu) = 
        H.tell Workspace.User.Menu.proxy 1 $ Workspace.User.Menu.Open

render { tmleft } = 
  HH.div_ 
  [ 
      HH.div [css "user-menu"] 
      [ Workspace.User.slot 0 HandleChildWorkspace, 
        Workspace.User.Menu.slot 1 HandleChildWorkspaceUser 
      ] 
  ]