module BCorrespondent.Page.Dashboard
  ( component
  , proxy
  )
  where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "home"

loc = "BCorrespondent.Page.Home"

component =
  H.mkComponent
    { initialState: identity
    , render:const render
    , eval: H.mkEval H.defaultEval
    }

render = HH.div [ css "loading-container" ] [ HH.text "dashboard" ]