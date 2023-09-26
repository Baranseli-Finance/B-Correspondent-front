module BCorrespondent.Page.ResetPassword (component, proxy, slot) where

import Prelude

import BCorrespondent.Component.Auth.SetPassword as Auth.SetPassword
import BCorrespondent.Data.Route (Route(Error500, Home))
import BCorrespondent.Capability.Navigate (navigate)

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.Auth.SignOut as SignOut
import BCorrespondent.Data.Home.Output
import BCorrespondent.Component.Async as Async
import Store (Action(WriteError))
import Halogen.Store.Monad (updateStore)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe (Nothing))

import Undefined

proxy = Proxy :: _ "reset_password"

loc = "BCorrespondent.Page.ResetPassword"

slot k = HH.slot_ proxy unit (component k) unit

data Action = HandleChild Auth.SetPassword.Output

component key =
  H.mkComponent
    { initialState: identity 
    , render:const $ render key
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (HandleChild (Auth.SetPassword.Server50x e)) = 
        updateStore (WriteError e) *> navigate Error500
      handleAction (HandleChild Auth.SetPassword.PasswordNotChanged) = 
        Async.send $ Async.mkOrdinary "password not changed" Async.Warning Nothing
      handleAction (HandleChild Auth.SetPassword.Ok) = do
        Async.send $ Async.mkOrdinary "password changed successfully" Async.Success Nothing
        navigate Home

render key = HH.div [ css "loading-container" ] [Async.slot 0, Auth.SetPassword.slot 1 key HandleChild  ]

