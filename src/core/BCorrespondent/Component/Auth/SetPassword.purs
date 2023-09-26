module BCorrespondent.Component.Auth.SetPassword
  ( Output(..)
  , component
  , proxy
  , slot
  )
  where

import Prelude

import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Capability.Navigate (navigate)
import BCorrespondent.Data.Route (Route (Error404))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Data.Maybe (Maybe (..), fromMaybe, isNothing)
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HPExt
import Data.Traversable (for_)
import Control.Alternative (guard)
import Halogen.Store.Monad (getStore, updateStore)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (removeItem)
import Web.HTML (window)
import Store (Action(UpdateJwtUser))
import Effect.Exception (Error)

import Undefined

proxy = Proxy :: _ "auth_set_password"

loc = "BCorrespondent.Component.Auth.SetPassword"

slot n s = HH.slot proxy n component {key: s}

data Output = Server50x Error | PasswordNotChanged | Ok

data Action =
       Initialize
     | MakeSetPassRequest Event 
     | FillPassword String 
     | FillAgainPassword String

type State = 
     { key :: String, 
       password :: Maybe String,
       againPassword :: Maybe String
     }

component =
  H.mkComponent
    { initialState: \{key} -> 
        { key: key, 
          password: Nothing, 
          againPassword: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize }
    }
    where
      handleAction Initialize = do 
        {user} <- getStore
        when (isNothing user) $ navigate Error404
      handleAction (MakeSetPassRequest ev) = do
        H.liftEffect $ preventDefault ev
        {key, password, againPassword} <- H.get
        let pass = do 
              x <- password
              y <- againPassword
              guard (x == y)
              pure x
        for_ pass \value -> do
          { config: Config { apiBCorrespondentHost: host }, user, jwtName } <- getStore
          for_ user \{token} -> do 
            resp <- Request.makeAuth (Just token) host Back.mkAuthApi $ 
              Back.setNewPassword 
                {key: key, password: value}
            let onError e = do 
                  H.modify_ _ { password = Nothing, againPassword = Nothing }
                  H.raise $ Server50x e
            onFailure resp onError \{success: isOK} ->
              if isOK 
              then do
                H.liftEffect $ 
                  window >>= 
                    localStorage >>= 
                      removeItem 
                        jwtName
                updateStore $ UpdateJwtUser Nothing
                H.raise Ok
              else 
                do H.modify_ _ { password = Nothing, againPassword = Nothing }
                   H.raise PasswordNotChanged
      handleAction (FillPassword x) = H.modify_ _ { password = Just x }
      handleAction (FillAgainPassword x) =  H.modify_ _ { againPassword = Just x }

render {key, password, againPassword} =
  HH.div_
  [
      HH.h4_ [ HH.text "Reset password" ]
  ,   if password == againPassword then HH.div_ []
      else 
        HH.div [ HPExt.style "margin-bottom: 10px;" ] 
               [ HH.span [ HPExt.style "color: red" ] 
                 [ HH.text "passwords mismatch" ] ]
  ,   HH.form [ HE.onSubmit MakeSetPassRequest ]
      [ 
          HH.div_
          [ HH.input
            [ HPExt.type_ HPExt.InputPassword
            , HE.onValueInput FillPassword
            , HPExt.value $ fromMaybe mempty password
            , HPExt.placeholder "password"
            ]
          ]
      ,   HH.div_
          [ HH.input
            [ HPExt.type_ HPExt.InputPassword
            , HE.onValueInput FillAgainPassword
            , HPExt.value $ fromMaybe mempty againPassword
            , HPExt.placeholder "again password"
            ]
          ]
      ,   HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "reset" ]
      ]
  ]