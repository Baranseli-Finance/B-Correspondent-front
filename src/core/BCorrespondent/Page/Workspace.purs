module BCorrespondent.Page.Workspace (Output (..), slot) where

import Prelude

import BCorrespondent.Component.Workspace.User as Workspace.User 
import BCorrespondent.Component.HTML.Utils (css, maybeElem)
import BCorrespondent.Component.Auth.SignOut as SignOut
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
        HandleChildSignOut SignOut.Output 
      | HandleChildResetLink SendResetPassLink.Output
      | HandleChildFileUploader FileUploader.Output

data Output = SignOutForward | RssetLinkForward | FilesUploaded (Array FileUploader.FileOutput)

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
        (HandleChildSignOut 
          SignOut.LoggedOut) = 
        H.raise SignOutForward
      handleAction 
        (HandleChildResetLink 
          (SendResetPassLink.ResetPasswordTimeLeft tmleft)) = 
        H.modify_ _ { tmleft = Just tmleft }
      handleAction 
        (HandleChildResetLink 
          (SendResetPassLink.ResetPasswordOk)) = do
        H.modify_ _ { tmleft = Nothing }
        H.raise RssetLinkForward
      handleAction 
        (HandleChildFileUploader 
          (FileUploader.FileIds fs)) = do
        H.raise $ FilesUploaded fs
        H.tell FileUploader.proxy 2 $ FileUploader.EraseFile

render { tmleft } = HH.div_ [ HH.div [css "user-menu"] [ Workspace.User.slot 1 unit ] ]