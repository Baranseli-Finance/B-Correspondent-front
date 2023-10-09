module BCorrespondent.Component.Workspace.Menu (slot, Output (..)) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Halogen.HTML.Events (onClick)

proxy = Proxy :: _ "workspace_menu"

loc = "BCorrespondent.Component.Workspace.Menu"

slot n = HH.slot proxy n component unit

data Output = Dashboard | History

data Action = OpenDashboard | OpenHistory

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction OpenDashboard = H.raise Dashboard
      handleAction OpenHistory = H.raise History

render = 
  HH.div [css "workspace-menu"] 
  [
      HH.div_
      [
          HH.ul 
          [css "ul-menu"] 
          [ 
              HH.li_
              [ 
                HH.div [onClick (const OpenDashboard)] [ HH.i [css "fa fa-home", HPExt.style "font-size:30px;color:white; cursor:pointer"] [] ]
              ]
          ,   HH.li [HPExt.style "padding-top:30px"]
              [ 
                HH.div [onClick (const OpenHistory)] [ HH.i [css "fa fa-history", HPExt.style "font-size:30px;color:white; cursor:pointer"] [] ]
              ]    
          ]
      ]
  ]
