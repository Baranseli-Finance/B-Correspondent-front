module BCorrespondent.Component.Auth.SignIn (Output (..), component, proxy, slot) where

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
import Data.Array as Array
import Data.Traversable (for_)
import Data.Int (fromString)

import Undefined

proxy = Proxy :: _ "auth_signIn"

loc = "BCorrespondent.Component.Auth.SignIn"

slot = HH.slot_ proxy unit component unit

data Output = LoggedInSuccess Jwt.JwtUser

data Action
  = MakeCodeRequest Event
  | MakeLoginRequest Event
  | MakeResendCodeRequest Event
  | Initialize
  | FillLogin String
  | FillPassword String
  | FillCode String

type State =
  { login :: Maybe String
  , password :: Maybe String
  , errMsg :: Maybe String
  , hash :: Maybe String
  , code :: Maybe Int
  }

component =
  H.mkComponent
    { initialState: 
      const { 
        login: Nothing, 
        password: Nothing, 
        errMsg: Nothing, 
        hash: Nothing :: Maybe String,
        code: Nothing :: Maybe Int }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = pure unit
  handleAction (FillLogin s) = H.modify_ _ { login = Just s }
  handleAction (FillPassword s) = H.modify_ _ { password = Just s }
  handleAction (MakeCodeRequest ev) = do
    H.liftEffect $ preventDefault ev
    s@{ login, password } <- H.get
    logDebug $ loc <> " state ---> " <> show s
    { config: Config { apiBCorrespondentHost }, browserFp } <- getStore
    let
      mkCred (Just login) (Just password)
        | length password > 0 =
            Just { login: login, password: password, browserFp: browserFp }
        | otherwise = Nothing
      mkCred _ _ = Nothing
    let cred = mkCred login password
    case cred of
      Nothing -> do
        H.modify_ _ { errMsg = Just "login or password is empty" }
        logDebug $ loc <> "login or password is empty"
      Just cred -> do
        resp <- Request.make apiBCorrespondentHost Back.mkAuthApi $ Back.sendCode cred
        let
          onError e =
            H.modify_ _
              { errMsg = Just $ message e
              , login = Nothing
              , password = Nothing
              }
        onFailure resp onError \{ success: hash :: String, warnings: ws } -> do
          logDebug $ loc <> " code hash ---> " <> hash
          if Array.length ws > 0
          then for_ ws \tmLeft ->
                 let msg = "the next attempt will be available in " <> tmLeft
                 in H.modify_ _
                    { errMsg = Just msg
                    , login = Nothing
                    , password = Nothing
                    }
          else H.modify_ _ { hash = Just hash, errMsg = Nothing }

  handleAction (FillCode c) = 
    case fromString c of 
      Just v -> H.modify_ _ { code = Just v, errMsg = Nothing }
      Nothing -> H.modify_ _ { code = Nothing, errMsg = Just $ "code must be a 6-digits number" }
  handleAction (MakeLoginRequest ev) = do
    H.liftEffect $ preventDefault ev
    {hash, code} <- H.get
    for_ code \c -> do 
      { config: Config { apiBCorrespondentHost } } <- getStore
      let codeBody = { code: c, hash: fromMaybe undefined hash }
      resp <- Request.make apiBCorrespondentHost Back.mkAuthApi $ Back.login codeBody
      let onError e = H.modify_ _ { errMsg = Just $ message e, code = Nothing }
      onFailure resp onError $ 
        \{success: token} -> do
            H.liftEffect $ 
              window >>= 
                localStorage >>= 
                  setItem "b-correspondent_jwt" token
            user <- H.liftEffect $ Jwt.parse token
            updateStore $ 
              UpdateJwtUser $ Just 
                { jwtUser: user, 
                  token: Back.JWTToken token 
                }
            H.raise $ LoggedInSuccess user
  handleAction (MakeResendCodeRequest ev) = do 
    H.liftEffect $ preventDefault ev
    {hash} <- H.get
    for_ hash \hash -> do 
      { config: Config { apiBCorrespondentHost }, browserFp } <- getStore
      let body = { browserFp: browserFp, hash: hash }
      resp <- Request.make apiBCorrespondentHost Back.mkAuthApi $ Back.resendCode body
      let onError e = H.modify_ _ { errMsg = Just $ message e }
      onFailure resp onError \{ success: hash :: String, warnings: ws } ->
        if Array.length ws > 0
        then for_ ws \tmLeft ->
               let msg = "the next attempt will be available in " <> tmLeft
               in H.modify_ _ { errMsg = Just msg }
        else H.modify_ _ { hash = Just hash, errMsg = Nothing }

render { login, password, errMsg, hash, code } =
  HH.div_
  [                            
      HH.h4_ [ HH.text "Sign In" ]
  ,   if isNothing errMsg then HH.div_ []
      else HH.div [ HPExt.style "margin-bottom: 10px;" ] [ HH.span [ HPExt.style "color: red" ] [ HH.text (fromMaybe undefined errMsg) ] ]
  ,   case hash of 
        Nothing ->
          HH.form [ HE.onSubmit MakeCodeRequest ]
          [ 
              HH.div_
              [ HH.input
                [ HPExt.type_ HPExt.InputText
                , HE.onValueInput FillLogin
                , HPExt.value $ fromMaybe mempty login
                , HPExt.placeholder "login"
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
          , HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "get code" ]
          ]
        Just _ ->
          HH.div_ 
          [
              HH.form [ HE.onSubmit MakeLoginRequest ] 
              [
                  HH.div_
                  [ HH.input
                    [ HPExt.type_ HPExt.InputText
                    , HE.onValueInput FillCode
                    , HPExt.value $ fromMaybe mempty $ map show code
                    , HPExt.placeholder "code"
                    ]
                  ]
              ,   HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "send code" ]
              ]
          ,   HH.form [ HE.onSubmit MakeResendCodeRequest ] 
              [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "haven't got code yet?" ] ]
          ]
  ]