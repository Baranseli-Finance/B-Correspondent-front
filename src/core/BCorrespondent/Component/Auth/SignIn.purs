module BCorrespondent.Component.Auth.SignIn
  ( component
  , proxy
  , slot
  ) where

import Prelude

import BCorrespondent.Data.Route as Route
import BCorrespondent.Capability.Navigate (navigate)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Component.HTML.Utils (css, safeHref)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, Event)
import Halogen.Store.Monad (getStore, updateStore)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.String (length)
import Effect.Exception (message)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem)
import Web.HTML (window)
import Store (Action(UpdateJwtUser))
import Crypto.Jwt as Jwt
import Effect.AVar as Async

import Undefined

proxy = Proxy :: _ "auth_signIn"

loc = "BCorrespondent.Component.Auth.SignIn"

slot = HH.slot_ proxy unit component unit

data Action
  = MakeRequest Event
  | Initialize
  | FillEmail String
  | FillPassword String

type State =
  { email :: Maybe String
  , password :: Maybe String
  , errMsg :: Maybe String
  }

component =
  H.mkComponent
    { initialState: const { email: Nothing, password: Nothing, errMsg: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = pure unit
  handleAction (FillEmail s) = H.modify_ _ { email = Just s }
  handleAction (FillPassword s) = H.modify_ _ { password = Just s }
  handleAction (MakeRequest ev) = do
    H.liftEffect $ preventDefault ev
    s@{ email, password } <- H.get
    logDebug $ loc <> " state ---> " <> show s
    { config: Config { apiBCorrespondentHost } } <- getStore
    let
      mkCred (Just email) (Just password)
        | length password > 0 =
            Just { email: email, password: password }
        | otherwise = Nothing
      mkCred _ _ = Nothing
    let cred = mkCred email password
    case cred of
      Nothing -> do
        H.modify_ _ { errMsg = Just "login or password is empty" }
        logDebug $ loc <> "login or password is empty"
      Just cred -> do
        resp <- Request.make apiBCorrespondentHost Back.mkAuthApi $ Back.login cred
        let
          onError e =
            H.modify_ _
              { errMsg = Just $ message e
              , email = Nothing
              , password = Nothing
              }
        onFailure resp onError \{ success: token :: String } -> do
          logDebug $ loc <> " jwt ---> " <> token
          H.liftEffect $ window >>= localStorage >>= setItem "b-correspondent_jwt" token
          user <- H.liftEffect $ Jwt.parse token
          updateStore $ UpdateJwtUser (Just { jwtUser: user, token: Back.JWTToken token })
          {isLoginVar} <- getStore
          void $ H.liftEffect $ Async.tryPut unit isLoginVar

render { email, password, errMsg } =
  HH.div_
  [                            
      HH.h4_ [ HH.text "Sign In" ]
  ,   if isNothing errMsg then HH.div_ []
      else HH.div [ HPExt.style "margin-bottom: 10px;" ] [ HH.span [ HPExt.style "color: red" ] [ HH.text (fromMaybe undefined errMsg) ] ]
  ,   HH.form [ HE.onSubmit MakeRequest ]
      [ 
          HH.div_
          [ HH.input
            [ HPExt.type_ HPExt.InputEmail
            , HE.onValueInput FillEmail
            , HPExt.value $ fromMaybe mempty email
            , HPExt.placeholder "email"
            ]
          ]
      ,   HH.div_
          [ HH.input
            [ HPExt.type_ HPExt.InputPassword
            , HE.onValueInput FillPassword
            , HPExt.value $ fromMaybe mempty password
            , HPExt.placeholder "password"
            ]
          ]
        , HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "submit" ]
      ]    
  ]