module BCorrespondent.Component.Workspace.Wallet.Withdraw (slot) where

import Prelude

import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Async as Async
import BCorrespondent.Component.HTML.Utils (css, maybeElem)
import BCorrespondent.Capability.LogMessages (logDebug, logError)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Web.Event.Event (preventDefault, Event)
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe (..), fromMaybe)
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Data.Traversable (for)
import Store (User)
import Effect.Exception (message, error)
import Data.Int (toNumber)
import Data.Array (sort, length, zip, (..))
import Data.Number (fromString)
import Data.Either (Either (..), isLeft, fromLeft)
import String.Regex (isValidNote)
import Data.Map as M
import Data.List (toUnfoldable)
import Data.Lens
import Foreign (unsafeFromForeign)

import Undefined

proxy = Proxy :: _ "workspace_wallet_withdraw"

loc = "BCorrespondent.Component.Wallet.Withdraw"

slot n = HH.slot_ proxy n component unit

data Action = 
       Initialize 
     | Withdraw Event 
     | FillAmount String 
     | SetCurrency Int
     | AddAll

type State = 
     { serverError :: Maybe String, 
       balances :: M.Map Int Back.Balance,
       amount :: Number,
       currencyidx :: Int,
       compError :: Maybe String
     }

component =
  H.mkComponent
    { initialState: 
      const 
       { 
         serverError: Nothing, 
         balances: M.empty,
         amount: 0.0,
         currencyidx: 0,
         compError: Nothing
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
    resp <- Request.makeAuth (Just token) host Back.mkInstitutionApi $ Back.initWithdrawal
    let failure e = H.modify_ _ { serverError = pure $ message e }
    onFailure resp failure \{success: { walletBalances }} ->
      let ys = 
               flip map walletBalances \x -> 
                 { currency: Back.decodeCurrency $ x^.Back._currencyB, 
                   amount: x^.Back._amountB,
                   walletIdent: x^.Back._walletIdent
                 }
      in H.modify_ _ { balances = M.fromFoldable (zip (0 .. length ys) (sort ys :: Array Back.Balance)) }
handleAction (Withdraw ev) = do 
  H.liftEffect $ preventDefault ev
  H.modify_ _ { compError = Nothing }
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do
    {amount, currencyidx, balances} <- H.get
    for_ (M.lookup currencyidx balances) \{walletIdent: ident, currency} -> do
      let body = { amount: amount, walletIdent: ident }
      resp <- Request.makeAuth (Just token) host Back.mkInstitutionApi $ Back.withdraw body
      let failure e = Async.send $ Async.mkException e loc
      onFailure resp failure \{success: {frozenFunds, status}} -> do 
        let withStatus _ Back.WithdrawalRegistered =
              let msg = 
                    "the request for withdrawal of " <> 
                    show amount <> " " <> 
                    show currency <> " has been submitted"
              in Async.send $ Async.mkOrdinary msg Async.Success Nothing
            withStatus _ Back.NotEnoughFunds = 
              Async.send $ Async.mkOrdinary "funds are not suffecient" Async.Warning Nothing
            withStatus amount Back.FrozenFunds = 
              let n = unsafeFromForeign amount :: Number
              in Async.send $ Async.mkOrdinary ("there are the frozen funds of " <> show n) Async.Warning Nothing
            withStatus _ Back.WithdrawResultStatusNotResolved = do 
              logError $ loc <> " cannot convert WithdrawResultStatus"
              Async.send $ Async.mkException (error ("cannot convert WithdrawResultStatus")) loc
        withStatus frozenFunds $ Back.decodeWithdrawResultStatus status
handleAction (FillAmount s) 
  | not (isValidNote s) = 
      H.modify_ _ { compError = pure "number in the format of {xxx.xx} is expected" }
handleAction (FillAmount s) = for_ (fromString s) \x -> H.modify_ _ { amount = x, compError = Nothing }
handleAction (SetCurrency idx) = do
  logDebug $ loc <> " set currency"
  H.modify_ _ { currencyidx = idx, compError = Nothing, amount = 0.0 }
handleAction AddAll = do 
  {currencyidx, balances} <- H.get
  for_ (M.lookup currencyidx balances) \{amount: value} -> 
    H.modify_ _ { amount = value }

render {serverError: Just msg } = HH.text msg
render {serverError: Nothing, balances } 
  | M.isEmpty balances = HH.h3_ [HH.text "there is no money on the accounts to be withdrawn"]
render {serverError: Nothing, amount, balances, compError } =
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

      ,   HH.span [HPExt.style "padding-right:10px;padding-left:5px"] []  
      ,   HH.span_
          [
              HH.select [HE.onSelectedIndexChange SetCurrency, HPExt.style "font-size:20px" ] $
              flip map (toUnfoldable (M.values balances)) \{currency} -> HH.option_ [ HH.text (show currency) ]
          ]
      ,   HH.span [HPExt.style "padding-right:10px;padding-left:5px"] []     
      ,   HH.span [ HE.onClick (const AddAll), css "withdraw-all" ] [HH.text "all"]
      ,   HH.span [HPExt.style "padding-right:10px;padding-left:5px"] []    
      ,   HH.span_ [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "withdraw", HPExt.style "cursor: pointer;font-size:20px" ] ]
      ]
  ,   HH.div [HPExt.style "padding-top:10px"] [] 
  ,   maybeElem compError \text -> HH.span [HPExt.style "color:red;font-size:20px;"] [HH.text text]    
  ]