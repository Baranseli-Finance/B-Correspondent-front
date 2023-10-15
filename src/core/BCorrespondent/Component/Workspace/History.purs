module BCorrespondent.Component.Workspace.History (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Array ((..))
import Web.Event.Event (preventDefault, Event)

import Undefined

proxy = Proxy :: _ "workspace_history"

loc = "BCorrespondent.Component.Workspace.History"

slot from to n = HH.slot_ proxy n (component from to) unit

data Action = 
       SetYear Int 
     | SetMonth Int 
     | SetDay Int 
     | MakeHistoryRequest Event

type State = { year :: Int, month :: Int, day :: Int, isInit :: Boolean, isLoading :: Boolean }

component from to =
  H.mkComponent
    { initialState: 
      const { 
        year: _.year from, 
        month: _.month from, 
        day: _.day from,
        isInit: true,
        isLoading: false
      }
    , render: render from to
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (SetYear idx) = logDebug $ loc <> "  ---> SetYear " <> show idx 
      handleAction (SetMonth idx) = logDebug $ loc <> "  ---> SetMonth " <> show idx
      handleAction (SetDay idx) = logDebug $ loc <> "  ---> SetDay " <> show idx
      handleAction (MakeHistoryRequest ev) = do 
        H.liftEffect $ preventDefault ev
        H.modify_ _ { isInit = false, isLoading = true }
        logDebug $ loc <> "  ---> history request made"
      
render from to {isInit, isLoading} = HH.div_ [selectors from to, timeline isInit isLoading]

selectors from to =
  HH.div 
  [css "history-selectors"]
  [   
      HH.div_
      [ HH.select [HE.onSelectedIndexChange SetYear ] $
        ((_.year from) .. (_.year to)) <#> \x -> HH.option_ [ HH.text (show x) ]
      ]
  ,   HH.div [HPExt.style "padding-left:10px"] 
      [ HH.select [HE.onSelectedIndexChange SetMonth ] $
        ((_.month from) .. (_.month to)) <#> \x -> HH.option_ [ HH.text (show x) ]
      ]
  ,   HH.div [HPExt.style "padding-left:10px"]
      [ HH.select [HE.onSelectedIndexChange SetDay ] $
        ((_.day from) .. (_.day to)) <#> \x -> HH.option_ [ HH.text (show x) ]
      ]
  ,   HH.div [HPExt.style "padding-left:10px"]
      [ 
        HH.form 
        [ HE.onSubmit MakeHistoryRequest ] 
        [ HH.input 
          [ HPExt.type_ HPExt.InputSubmit, 
            HPExt.style "cursor:pointer", 
            HPExt.value "load timeline"
          ] 
        ]
      ]   
  ]

timeline isInit isLoading = 
  HH.div [css "history-timeline"] $ 
  if isInit 
  then textContainer "no timeline loaded"
  else if isLoading
  then textContainer "loading..."
  else [HH.text "...."]
  
textContainer text = [HH.div [HPExt.style "position:absolute;left:45%;top:40%"] [HH.h3 [HPExt.style "text-transform: uppercase"] [HH.text text]]]
