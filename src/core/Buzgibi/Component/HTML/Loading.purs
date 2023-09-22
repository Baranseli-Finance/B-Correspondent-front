-- https://codepen.io/DHawku/pen/yXEZdq
module Buzgibi.Component.HTML.Loading (html) where

import Buzgibi.Component.HTML.Utils (css)

import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt

html =
  HH.div [ css "loading-container" ]
    [ HH.div [ css "waviy" ]
        [ HH.span [ HPExt.style "--i:1" ] [ HH.text "l" ]
        , HH.span [ HPExt.style "--i:2" ] [ HH.text "o" ]
        , HH.span [ HPExt.style "--i:3" ] [ HH.text "a" ]
        , HH.span [ HPExt.style "--i:4" ] [ HH.text "d" ]
        , HH.span [ HPExt.style "--i:5" ] [ HH.text "i" ]
        , HH.span [ HPExt.style "--i:6" ] [ HH.text "n" ]
        , HH.span [ HPExt.style "--i:7" ] [ HH.text "g" ]
        , HH.span [ HPExt.style "--i:8" ] [ HH.text "." ]
        , HH.span [ HPExt.style "--i:9" ] [ HH.text "." ]
        , HH.span [ HPExt.style "--i:10" ] [ HH.text "." ]
        ]
    ]