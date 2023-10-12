module BCorrespondent.Component.Workspace.Dashboard.Transaction ( Query(..), slot, proxy ) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Data.Functor (($>))

proxy = Proxy :: _ "workspace_dashboard_transaction"

loc = "BCorrespondent.Component.Workspace.Dashboard.Transaction"

slot n = HH.slot_ proxy n component unit

data Query a = Open Int a

data Action = Close Event

type State = { isOpen :: Boolean }

component =
  H.mkComponent
    { initialState: const { isOpen: false }
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
      handleQuery (Open _ a) = H.modify_ _ { isOpen = true } $> Just a
      handleAction (Close ev) = H.liftEffect (preventDefault ev) *> H.modify_ _ { isOpen = false }

render {isOpen} = HH.div [css "transaction-modal-container", HPExt.style display ] [transaction]
  where display = if isOpen then "display:block" else "display:none"

transaction = 
  HH.div [css "transaction-modal"] 
  [  HH.text "transaction info"
  ,  HH.form [ HE.onSubmit Close ] [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "close", HPExt.style "cursor: pointer" ] ]
  ]