module Buzgibi.Page.Error.Page404 (component, proxy) where

import Prelude

import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Data.Route (Route(Home))

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HPExt

proxy = Proxy :: _ "error404"

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }

render =
  HH.section [ css "centered" ]
    [ HH.h1_ [ HH.text "Opps! 404" ]
    , HH.div [ css "container" ]
        [ HH.span [ HPExt.style "font-size:20px;" ]
            [ HH.text "We couldn't find the requested page" ]
        , HH.a [ safeHref Home, HPExt.style hrefStyle ] [ HH.text "HOME" ]
        ]
    ]

hrefStyle = "font-size:20px; margin-left:10px; text-decoration:none; color:black"