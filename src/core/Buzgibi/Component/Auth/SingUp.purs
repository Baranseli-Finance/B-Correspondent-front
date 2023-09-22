module Buzgibi.Component.Auth.SignUp
  ( component
  , proxy
  , slot
  ) where

import Prelude

import Buzgibi.Component.Auth.SignUp.PasswordCheker (Validation, check)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request.Handler (onFailure)
import Buzgibi.Data.Config (Config(..))
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route
import Buzgibi.Component.HTML.Utils (css, safeHref)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust)
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Data.Traversable (for)
import Data.String (length)
import Halogen.Store.Monad (getStore, updateStore)
import Effect.Exception (message)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem)
import Web.HTML (window)
import Store (Action(UpdateJwtUser))
import Crypto.Jwt as Jwt
import Safe.Coerce

import Undefined

loc = "Buzgibi.Component.Auth.SignUp"

proxy = Proxy :: _ "auth_signUp"

data Error = Mismatch | Weak String

slot = HH.slot_ proxy unit component unit

data Action
  = MakeRequest Event
  | FillEmail String
  | FillPassword String
  | FillRepeatedPassword String
  | Initialize

type State =
  { email :: Maybe String
  , password :: Maybe String
  , reapatedPassword :: Maybe String
  , strength :: Maybe Validation
  , errMsg :: Maybe String
  }

def =
  { email: Nothing
  , password: Nothing
  , reapatedPassword: Nothing
  , strength: Nothing
  , errMsg: Nothing
  }

component =
  H.mkComponent
    { initialState: const def
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = do
    { user } <- getStore
    when (isJust user) $ navigate Route.Home
  handleAction (MakeRequest ev) = do
    H.liftEffect $ preventDefault ev
    logDebug $ loc <> "pass stregth ---> trying registering"
    { email, password, reapatedPassword } <- H.get
    if password /= reapatedPassword then do
      H.modify_ _ { errMsg = Just "passwords mismatch" }
      logDebug $ loc <> "passwords mismatch"
    else do
      { config: Config { apiBuzgibiHost } } <- getStore
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
          resp <- Request.make apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.register cred
          let
            onError e =
              H.modify_ _
                { errMsg = Just $ message e
                , email = Nothing
                , password = Nothing
                , reapatedPassword = Nothing
                , strength = Nothing
                }
          onFailure resp onError \{ success: token :: String } -> do
            logDebug $ loc <> " jwt ---> " <> token
            H.liftEffect $ window >>= localStorage >>= setItem "buzgibi_jwt" token
            user <- H.liftEffect $ Jwt.parse $ coerce token
            updateStore $ UpdateJwtUser (Just { jwtUser: user, token: BuzgibiBack.JWTToken token })
            navigate Route.Home
  handleAction (FillEmail s) = H.modify_ _ { email = Just s }
  handleAction (FillPassword s) = do
    logDebug $ loc <> "pass stregth ---> " <> show (check s)
    H.modify_ _ { password = Just s, strength = Just (check s) }
  handleAction (FillRepeatedPassword s) = H.modify_ _ { reapatedPassword = Just s }

render { email, password, reapatedPassword, strength, errMsg } =
  HH.div [ css "col-12 text-center align-self-center py-5" ]
    [ HH.div [ css "section pb-5 pt-5 pt-sm-2 text-center" ]
        [ HH.div [ css "card-3d-wrap mx-auto" ]
            [ HH.div [ css "card-3d-wrapper" ]
                [ HH.div [ css "card-front" ]
                    [ HH.div [ css "center-wrap" ]
                        [ HH.div [ css "section text-center" ]
                            [ HH.h4 [ css "mb-4 pb-3", HPExt.style "margin-top: 30px;" ] [ HH.text "Sign Up" ]
                            , if isNothing errMsg then HH.div_ []
                              else HH.div [ HPExt.style "margin-bottom: 10px;" ] [ HH.span [ HPExt.style "color: red" ] [ HH.text (fromMaybe undefined errMsg) ] ]
                            , HH.form [ HE.onSubmit MakeRequest ]
                                [ HH.div [ css "form-group" ]
                                    [ HH.input
                                        [ css "form-style"
                                        , HPExt.type_ HPExt.InputEmail
                                        , HE.onValueInput FillEmail
                                        , HPExt.value $ fromMaybe mempty email
                                        , HPExt.placeholder "email"
                                        ]
                                    , HH.i [ css "input-icon uil uil-at" ] []
                                    ]
                                , if
                                    isNothing password ||
                                      (map length password == Just 0) then HH.div_ []
                                  else HH.div_ $ [ HH.text (fromMaybe mempty (map _.value strength)) ]
                                , HH.div [ css "form-group mt-2" ]
                                    [ HH.input
                                        [ css "form-style"
                                        , HPExt.type_ HPExt.InputPassword
                                        , HE.onValueInput FillPassword
                                        , HPExt.value $ fromMaybe mempty password
                                        , HPExt.placeholder "password"
                                        ]
                                    , HH.i [ css "input-icon uil uil-lock-alt" ] []
                                    ]
                                , HH.div [ css "form-group mt-2" ]
                                    [ HH.input
                                        [ css "form-style"
                                        , HPExt.type_ HPExt.InputPassword
                                        , HE.onValueInput FillRepeatedPassword
                                        , HPExt.value $ fromMaybe mempty reapatedPassword
                                        , HPExt.placeholder "repeat password"
                                        ]
                                    , HH.i [ css "input-icon uil uil-lock-alt" ] []
                                    ]
                                , HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "submit" ]
                                , HH.p [ css "mb-0 mt-4 text-center" ] [ HH.a [ css "link", safeHref Route.SignIn ] [ HH.text "Sign In" ] ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]