module BCorrespondent.Component.Workspace.Wallet.Order (slot) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "workspace_wallet_order"

loc = "BCorrespondent.Component.Wallet.Order"

slot n = HH.slot_ proxy n component unit

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }


render = HH.text "order component"
