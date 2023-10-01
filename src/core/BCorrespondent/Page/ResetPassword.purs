module BCorrespondent.Page.ResetPassword (component, proxy, slot) where

import Prelude

import BCorrespondent.Component.Auth.SetPassword as Auth.SetPassword
import BCorrespondent.Data.Route (Route(Error500, Home))
import BCorrespondent.Capability.Navigate (navigate)
import BCorrespondent.Component.Timer as Timer 

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.Auth.SignOut as SignOut
import BCorrespondent.Data.ChildOutput.Home
import BCorrespondent.Component.Async as Async
import Store (Action(WriteError))
import Halogen.Store.Monad (updateStore)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe (Nothing))
import Effect.Aff as Aff

import Undefined

proxy = Proxy :: _ "reset_password"

loc = "BCorrespondent.Page.ResetPassword"

slot k = HH.slot_ proxy unit (component k) unit

data Action = HandleChild Auth.SetPassword.Output

type State = { isOk :: Boolean }

component key =
  H.mkComponent
    { initialState: const { isOk: false }
    , render: render key
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (HandleChild (Auth.SetPassword.Server50x e)) = 
        updateStore (WriteError e) *> navigate Error500
      handleAction (HandleChild Auth.SetPassword.PasswordNotChanged) = 
        Async.send $ Async.mkOrdinary "password not changed" Async.Warning Nothing
      handleAction (HandleChild Auth.SetPassword.Ok) = do
        H.modify_ _ { isOk = true }
        H.liftAff $ Aff.delay $ Aff.Milliseconds 5000.0
        navigate Home

render key { isOk } = 
  HH.div 
  [ css "loading-container" ] 
  [ Async.slot 0,
    if isOk 
    then HH.text "password changed successfully"
    else Auth.SetPassword.slot 1 key HandleChild  
  ]

