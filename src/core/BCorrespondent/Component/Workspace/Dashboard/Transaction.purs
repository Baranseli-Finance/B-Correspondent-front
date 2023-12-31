module BCorrespondent.Component.Workspace.Dashboard.Transaction
  ( Query(..)
  , proxy
  , slot
  , transactionWinFailure
  , transactionWinOk
  )
  where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, maybeElem)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (getStore)
import Web.Event.Event (preventDefault, Event)
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Data.Functor (($>))
import Data.Traversable (for)
import Data.Lens
import Date.Format (format)
import Control.Alt (alt)
import Data.Maybe (fromMaybe)


import Undefined

proxy = Proxy :: _ "workspace_dashboard_transaction"

loc = "BCorrespondent.Component.Workspace.Dashboard.Transaction"

slot n = HH.slot_ proxy n component unit

data Query a = Open Int a

data Action = Close Event

type State = 
    { isOpen :: Boolean, 
      transaction :: Maybe Back.NoForeignTransaction
    }

component =
  H.mkComponent
    { initialState: 
      const { isOpen: false, transaction: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
    }
    where
      handleQuery 
        :: forall a s . Query a
        -> H.HalogenM State Action s Unit AppM (Maybe a)
      handleQuery (Open ident a) = do 
        { config: Config { apiBCorrespondentHost: host }, user, jwtName } <- getStore
        map (const (Just a)) $ 
          for user \{ token } -> do
            resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ Back.fetchTrnsaction ident
            let failure e = Async.send $ Async.mkException e loc
            onFailure resp failure \{ success: foreignTr :: Back.ForeignTransaction } -> do
               let res = Back.resolveTransaction foreignTr
               onFailure res failure \tr@{ok, failure} -> do
                 oktm <- H.liftEffect $ for ok \x -> do
                   ftm <- format $ x^.Back._timestamp
                   pure $ x # Back._timestamp .~ ftm
                 failuretm <- H.liftEffect $ for failure \x -> do
                   ftm <- format $ x^.Back._timestamp
                   pure $ x # Back._timestamp .~ ftm
                 logDebug $ loc <> " transaction ---> " <> show tr
                 H.modify_ _ { isOpen = true, transaction = Just { ok: oktm, failure: failuretm } }
      handleAction (Close ev) = H.liftEffect (preventDefault ev) *> H.modify_ _ { isOpen = false }

render {isOpen, transaction: value} =
  HH.div 
  [css "transaction-modal-container", 
   HPExt.style display]
  [maybeElem value \{ok, failure} -> 
    fromMaybe (HH.div_ [HH.text "not transaction to be rendered"]) $
      (ok <#> \x -> HH.div [css "transaction-modal-ok"] $ transactionWinOk x <> [closeButton]) `alt`
      (failure <#> \x -> HH.div [css "transaction-modal-failure"] $ transactionWinFailure x <> [closeButton])
  ]
  where display = if isOpen then "display:block" else "display:none"

transactionWinOk value =
  [  HH.div [HPExt.style "padding-top:20px"] [HH.h3_ [HH.text "transaction details"]]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "sender:  "]
     , HH.span_ [HH.text $ value^.Back._sender]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "country:  "]
     , HH.span_ [HH.text $ show $ value^.Back._senderCountry]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "city:  "]
     , HH.span_ [HH.text $ value^.Back._senderCity]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "sender bank:  "]
     , HH.span_ [HH.text $ value^.Back._senderBank]
     ]
--   ,  HH.div 
--      [HPExt.style "padding-top:20px"] 
--      [ HH.span_ [HH.text "recipient:  "]
--      , HH.span_ [HH.text $ value^.Back._receiver]
--      ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "recipient bank:  "]
     , HH.span_ [HH.text $ value^.Back._receiverBank]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "amount:  "]
     , HH.span_ [HH.text $ show $ value^.Back._amount]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "currency:  "]
     , HH.span_ [HH.text $ show $ value^.Back._currency]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "description:  "]
     , HH.span_ [HH.text $ value^.Back._description]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "charges:  "]
     , HH.span_ [HH.text $ show $ value^.Back._charges]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "tm:  "]
     , HH.span_ [HH.text $ value^.Back._timestamp]
     ]
  ]

transactionWinFailure value = 
  [  HH.div [HPExt.style "padding-top:20px"] [HH.h3_ [HH.text "transaction details"]]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "reason:  "]
     , HH.span_ [HH.text $ value^.Back._reason]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "tm:  "]
     , HH.span_ [HH.text $ value^.Back._timestamp]
     ]   
  ]

closeButton = 
  HH.form
  [HPExt.style "padding-top:40px", HE.onSubmit Close ] 
  [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "close", HPExt.style "cursor: pointer;font-size:20px" ] ]