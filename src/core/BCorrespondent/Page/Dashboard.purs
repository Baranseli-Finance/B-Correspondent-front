module BCorrespondent.Page.Dashboard (Output (..), slot) where

import Prelude

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

proxy = Proxy :: _ "dashboard"

loc = "BCorrespondent.Page.Dashboard"

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

render { tmleft } = 
  HH.div 
  [ css "loading-container" ] 
  [ HH.text "dashboard",
    SignOut.slot 0 HandleChildSignOut,
    SendResetPassLink.slot 1 HandleChildResetLink,
    FileUploader.slot 2 "test" HandleChildFileUploader,
    maybeElem tmleft \left ->
      HH.div 
      [ HPExt.style "margin-bottom: 10px;" ] 
      [ HH.span [ HPExt.style "color: red" ] 
        [ HH.text $ "the next attempt is in " <> show left <> " sec" 
        ]
      ]
  ]