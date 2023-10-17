module BCorrespondent.Component.Workspace.Wallet (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.Workspace.Wallet.Order as Order
import BCorrespondent.Component.Workspace.Wallet.Withdraw as Withdraw 

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Generic.Rep (class Generic)

proxy = Proxy :: _ "workspace_wallet"

loc = "BCorrespondent.Component.Workspace.Wallet"

slot n = HH.slot_ proxy n component unit

data Kind = Withdraw | Order

derive instance genericKind :: Generic Kind _
derive instance eqKind :: Eq Kind

data Action = ChooseKind Kind 

type State = { kind :: Kind }

component =
  H.mkComponent
    { initialState: const { kind: Withdraw }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where 
      handleAction (ChooseKind kind) = H.modify_ _ { kind = kind }

render {kind} =
  HH.div_ 
  [
      HH.div [css "wallet-menu-container"] 
      [ 
          HH.span 
          [HE.onClick (const (ChooseKind Withdraw)), 
           css "wallet-action", 
           HPExt.style $ if kind == Withdraw then selected else released
          ] [HH.text "withdraw money"]
      ,   HH.span 
          [HE.onClick (const (ChooseKind Order)), 
           css "wallet-action", 
           HPExt.style $ if kind == Order then selected else released
          ] [HH.text "transaction order"]
      ]
  ,   HH.div [css "wallet-menu-container-body"] 
      [if kind == Withdraw then Withdraw.slot 1 else Order.slot 1 ]
  ]
  where
    selected = "cursor:not-allowed;background-color:#d8d8d8;"
    released = "cursor:pointer;background-color:#aeaeae;"