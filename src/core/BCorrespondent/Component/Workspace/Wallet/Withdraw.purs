module BCorrespondent.Component.Workspace.Wallet.Withdraw (slot) where

import Prelude

import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Async as Async
import BCorrespondent.Component.HTML.Utils (css, maybeElem, whenElem)
import BCorrespondent.Capability.LogMessages (logDebug, logError)
import BCorrespondent.Component.Pagination as Pagination
import BCorrespondent.Component.Subscription.WS as WS

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Web.Event.Event (preventDefault, Event)
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe (..), fromMaybe, isNothing, isJust)
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Data.Traversable (for)
import Store (User)
import Effect.Exception (message, error)
import Data.Int (toNumber)
import Data.Array (sort, length, zip, (..), head, (:), findIndex, insertAt)
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
     | HandleChildPagination Pagination.Output
     | UpdateWithdrawalhistoryItem Back.WithdrawalHistory

type State = 
     { serverError :: Maybe String, 
       balances :: M.Map Int Back.Balance,
       history :: Array Back.WithdrawalHistoryItem,
       amount :: Maybe Number,
       currencyidx :: Int,
       compError :: Maybe String,
       perPage :: Int,
       total :: Int
     }

component =
  H.mkComponent
    { initialState: 
      const 
       { 
         serverError: Nothing, 
         balances: M.empty,
         history: [],
         amount: Nothing,
         currencyidx: 0,
         compError: Nothing,
         perPage: 0,
         total: 0
       }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }
  where
    handleAction Initialize = do 
      { config: Config { apiBCorrespondentHost: host }, user } <- getStore
      for_ (user :: Maybe User) \{ token } -> do 
        resp <- Request.makeAuth (Just token) host Back.mkInstitutionApi $ Back.initWithdrawal
        let failure e = H.modify_ _ { serverError = pure $ message e }
        onFailure resp failure \{success: { walletBalances, history: {items, total} }} -> do
          let ys = 
                  flip map walletBalances \x -> 
                    { currency: Back.decodeCurrency $ x^.Back._currencyB, 
                      amount: x^.Back._amountB,
                      walletIdent: x^.Back._walletIdent
                    }
              zs = 
                  flip map items \x -> 
                    { currency: 
                        Back.decodeCurrency $ 
                          x^.Back._currencyW, 
                      amount: x^.Back._amountW,
                      withdrawalStatus: 
                        Back.decodeWithdrawalStatus $ 
                          x^.Back._withdrawalStatus,
                      initiator: x^.Back._initiator,
                      created: x^.Back._created,
                      ident: x^.Back._ident
                    }
          H.modify_ _ 
            { balances = M.fromFoldable (zip (0 .. length ys) (sort ys :: Array Back.Balance)), 
              history = (zs :: Array Back.WithdrawalHistoryItem),
              perPage = 10,
              total = total
            }
          WS.subscribe loc WS.withdrawalUrl (Just (WS.encodeResource WS.Withdrawal)) 
            \{success: history} -> handleAction $ UpdateWithdrawalhistoryItem (history :: Back.WithdrawalHistory)
    handleAction (Withdraw ev) = do 
      H.liftEffect $ preventDefault ev
      {compError, amount} <- H.get
      when (isNothing compError && isNothing amount) $ H.modify_ _ { compError = pure "number not specified"}
      when (isNothing compError && 
            isJust amount) $ do
        H.modify_ _ { compError = Nothing }
        { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        for_ (user :: Maybe User) \{ token } -> do
          {currencyidx, balances} <- H.get
          for_ (M.lookup currencyidx balances) \{walletIdent: ident, currency} -> do
            let body = { amount: fromMaybe 0.0 amount, walletIdent: ident }
            resp <- Request.makeAuth (Just token) host Back.mkInstitutionApi $ Back.withdraw body
            let failure e = Async.send $ Async.mkException e loc
            onFailure resp failure \{success: {frozenFunds, status}} -> do 
              let withStatus _ Back.WithdrawalRegistered =
                    let msg = 
                          "the request for withdrawal of " <> 
                          show (fromMaybe 0.0 amount) <> " " <> 
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
    handleAction (FillAmount s) = for_ (fromString s) \x -> H.modify_ _ { amount = Just x, compError = Nothing }
    handleAction (SetCurrency idx) = do
      logDebug $ loc <> " set currency"
      H.modify_ _ { currencyidx = idx, compError = Nothing, amount = Nothing }
    handleAction AddAll = do 
      {currencyidx, balances} <- H.get
      for_ (M.lookup currencyidx balances) \{amount: value} -> 
        H.modify_ _ { amount = Just value }
    handleAction (HandleChildPagination (Pagination.Page page)) = do 
      { config: Config { apiBCorrespondentHost: host }, user } <- getStore
      for_ (user :: Maybe User) \{ token } -> do
        resp <- Request.makeAuth (Just token) host Back.mkInstitutionApi $ 
          Back.fetchWithdrawHistoryPage page
        let failure e = Async.send $ Async.mkException e loc
        onFailure resp failure \{success: {items, total}} -> do
          let xs = 
                  flip map items \x -> 
                    { currency: 
                        Back.decodeCurrency $ 
                        x^.Back._currencyW, 
                      amount: x^.Back._amountW,
                      withdrawalStatus: 
                        Back.decodeWithdrawalStatus $ 
                          x^.Back._withdrawalStatus,
                      initiator: x^.Back._initiator,
                      created: x^.Back._created,
                      ident: x^.Back._ident
                    }
          H.modify_ _ { history = xs, total = total }          
    handleAction (UpdateWithdrawalhistoryItem {total, items}) =
      for_ (head items) \x -> do
        {history} <- H.get
        let item = 
              { currency: 
                  Back.decodeCurrency $ 
                    x^.Back._currencyW, 
                amount: x^.Back._amountW,
                withdrawalStatus: 
                  Back.decodeWithdrawalStatus $ 
                    x^.Back._withdrawalStatus,
                initiator: x^.Back._initiator,
                created: x^.Back._created,
                ident: x^.Back._ident
              }
        let idx = flip findIndex history $ \y -> _.ident y == _.ident x
        let newHistory = fromMaybe (item : history) $ join $ flip map idx \i -> insertAt i item history
        H.modify_ \s -> s { history = newHistory, total = total } 


render {serverError: Just msg } = HH.text msg
render {serverError: Nothing, balances } 
  | M.isEmpty balances = HH.h3_ [HH.text "there is no money on the accounts to be withdrawn"]
render {serverError: Nothing, amount, balances, compError, history, total, perPage } =
  HH.div_
  [
      HH.form [ HE.onSubmit Withdraw ]
      [
          HH.span_
          [
            HH.input
            [ HPExt.type_ HPExt.InputText
            , HE.onValueInput FillAmount
            , HPExt.value $ show $ fromMaybe 0.0 amount
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
  ,   whenElem (length history > 0) $ HH.div [css "withdraw-history-container"] $ renderHistory history
  ,   whenElem (total >= perPage) $ Pagination.slot 1 { total: total, perpage: perPage, page: 1 } HandleChildPagination
  ]

renderHistory xs = 
  xs <#> \{currency, initiator, amount, created, withdrawalStatus} ->
    HH.div_ 
    [
        HH.span [css "withdraw-history-container-item"] [HH.text (show amount)],
        HH.span [css "withdraw-history-container-item"] [HH.text (show currency)],
        HH.span [css "withdraw-history-container-item"] [HH.text (show withdrawalStatus)],
        HH.span [css "withdraw-history-container-item"] [HH.text initiator],
        HH.span [css "withdraw-history-container-item"] [HH.text created],
        HH.div [HPExt.style "padding-top:10px"] []
    ]