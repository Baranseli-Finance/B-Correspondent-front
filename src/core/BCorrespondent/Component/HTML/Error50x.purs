module BCorrespondent.Component.HTML.Error50x (html) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt

html error =
  HH.section [ HPExt.style "height: 100vh;display: flex;flex-direction: column;justify-content: center;align-items: center;" ]
    [ HH.h1_ [ HH.text "Internal Server Error or Network failure" ]
    , HH.div 
      [ HPExt.style "text-align: center;vertical-align: middle;font-szie: 20pxfont-color: red" ] 
      [ HH.span [ HPExt.style "font-size:20px; color:red;" ] 
        [ HH.text ("There is a technical hitch: " <> error) ] 
      ]
    , HH.div_ [HH.a [HPExt.href "mailto:admin@b-correspondent.app", HPExt.style "text-decoration:none"] [HH.h3_ [HH.text "let us know about it"]]]
    ]