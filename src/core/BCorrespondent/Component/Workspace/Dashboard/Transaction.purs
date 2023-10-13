module BCorrespondent.Component.Workspace.Dashboard.Transaction ( Query(..), slot, proxy ) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, maybeElem)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug)

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

import Undefined

proxy = Proxy :: _ "workspace_dashboard_transaction"

loc = "BCorrespondent.Component.Workspace.Dashboard.Transaction"

slot n = HH.slot_ proxy n component unit

data Query a = Open Int a

data Action = Close Event

type State = 
    { isOpen :: Boolean, 
      transaction :: Maybe Back.Transaction 
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
            let failure _ = undefined
            onFailure resp failure \{ success: x :: Back.Transaction } -> do 
              logDebug $ loc <> " transaction ---> " <> show x
              H.modify_ _ { isOpen = true, transaction = Just x }
      handleAction (Close ev) = H.liftEffect (preventDefault ev) *> H.modify_ _ { isOpen = false }

render {isOpen, transaction: value} = 
  HH.div 
  [css "transaction-modal-container", 
   HPExt.style display]
  [maybeElem value transactionWin]
  where display = if isOpen then "display:block" else "display:none"

transactionWin value = 
  HH.div [css "transaction-modal"] 
  [  HH.div [HPExt.style "padding-top:20px"] [HH.h3_ [HH.text "transaction details"]]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "transaction id:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._ident]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "amount:  "]
     , HH.span_ [HH.text $ show $ value^.Back._transaction <<< Back._amount]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "currency:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._currency]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "sender:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._senderName]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "sender address:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._senderAddress]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "sender phone:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._senderPhoneNumber]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "sender bank:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._senderBank]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "sender bank account:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._senderBankAccount]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "swift code:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._swiftSepaCode]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "correspondent bank:  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._correspondentBank]
     ]
  ,  HH.div 
     [HPExt.style "padding-top:20px"] 
     [ HH.span_ [HH.text "swift code (correspondent bank):  "]
     , HH.span_ [HH.text $ value^.Back._transaction <<< Back._correspondentBankSwiftSepaCode]
     ]      
  ,  HH.form 
     [HPExt.style "padding-top:40px", HE.onSubmit Close ] 
     [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "close", HPExt.style "cursor: pointer" ] ]
  ]