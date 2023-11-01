module BCorrespondent.Page.ResetPassword (component, proxy, slot) where

import Prelude

import BCorrespondent.Component.Auth.SetPassword as Auth.SetPassword
import BCorrespondent.Data.Route (Route(Error500, Home))
import BCorrespondent.Capability.Navigate (navigate)
import BCorrespondent.Component.Timer as Timer 
import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.Auth.SignOut as SignOut
import BCorrespondent.Component.Async as Async
import BCorrespondent.Component.HTML.Utils (chooseElem)

import Store (Action(WriteError))
import Halogen.Store.Monad (updateStore)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe (..), fromMaybe)
import Effect.Aff as Aff

import Undefined

proxy = Proxy :: _ "reset_password"

loc = "BCorrespondent.Page.ResetPassword"

slot k = HH.slot_ proxy unit (component k) unit

data Action = 
       HandleChildSetPass Auth.SetPassword.Output 
     | HandleChildResetPasswordLink Timer.Output

type State = { isOk :: Boolean, timeleft :: Maybe Int }

component key =
  H.mkComponent
    { initialState: 
       const 
       { isOk: false, 
         timeleft: Nothing 
       }
    , render: render key
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (HandleChildSetPass (Auth.SetPassword.Server50x e)) = 
        updateStore (WriteError e) *> navigate Error500
      handleAction (HandleChildSetPass Auth.SetPassword.PasswordNotChanged) = 
        Async.send $ 
          Async.mkOrdinary 
            "password not changed" 
            Async.Warning 
            Nothing
      handleAction (HandleChildSetPass Auth.SetPassword.PasswordOk) =
        H.modify_ _ { isOk = true }
      handleAction (HandleChildResetPasswordLink (Timer.Emit sec)) = do
        when (sec == 0) $ navigate Home
        H.modify_ _ { timeleft = Just sec }

render key { isOk, timeleft } =
  HH.div 
  [ css "password-reset-container" ] 
  [ Async.slot 0, chooseElem isOk ok no ]
  where 
    ok =  
      HH.div_ 
      [ Timer.slot 1 {interval: 5} HandleChildResetPasswordLink, 
        HH.text $ 
          "password changed successfully. \
          \ You'll be redirected to home in " <> 
          show (fromMaybe 5 timeleft) <> " sec"
      ]
    no = HH.div_  [Auth.SetPassword.slot 1 key HandleChildSetPass]