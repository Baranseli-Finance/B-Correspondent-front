module BCorrespondent.Component.Workspace.Wallet.Withdraw (slot) where

import Prelude

import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Web.Event.Event (preventDefault, Event)
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe (..))
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Store (User)
import Effect.Exception (message)
import Data.Int (toNumber)

proxy = Proxy :: _ "workspace_wallet_withdraw"

loc = "BCorrespondent.Component.Wallet.Withdraw"

slot n = HH.slot_ proxy n component unit

data Action = Initialize | Withdraw Event | FillAmount String

type State = { error :: Maybe String, balances :: Array Back.Balance, amount :: Number }

component =
  H.mkComponent
    { initialState: 
      const 
       { 
         error: Nothing, 
         balances: [], 
         amount: 0.0 
       }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }

handleAction Initialize = do 
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do 
    resp <- Request.makeAuth (Just token) host Back.mkInstitutionApi $ Back.fetchBalances
    let failure e = H.modify_ _ { error = pure $ message e }
    onFailure resp failure \{success: { xs }} -> H.modify_ _ { balances = xs }
handleAction (Withdraw ev) = do 
  H.liftEffect $ preventDefault ev
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do
    let body = { amount: toNumber 0, currency: Back.EUR }
    resp <- Request.makeAuth (Just token) host Back.mkInstitutionApi $ Back.withdraw body
    let failure e = Async.send $ Async.mkException e loc
    onFailure resp failure \{success: _} -> pure unit
handleAction (FillAmount _) = pure unit

render {error: Just msg } = HH.text msg
render {error: Nothing, amount } = 
  HH.div_ 
  [
      HH.form [ HE.onSubmit Withdraw ]
      [
          HH.span_
          [
            HH.input
            [ HPExt.type_ HPExt.InputText
            , HE.onValueInput FillAmount
            , HPExt.value $ show amount
            , HPExt.style "font-size:20px"
            , HPExt.size 16
            , HPExt.maxLength 16
            ]
          ]
      ,   HH.span [HPExt.style "padding-right:10px;padding-left:10px"] []    
      ,   HH.span_ [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "withdraw", HPExt.style "cursor: pointer;font-size:20px" ] ]
      ]
  ]