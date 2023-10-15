module BCorrespondent.Component.Workspace.History (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Array ((..), index)
import Web.Event.Event (preventDefault, Event)
import Data.Foldable (for_)

import Undefined

proxy = Proxy :: _ "workspace_history"

loc = "BCorrespondent.Component.Workspace.History"

slot from to n = HH.slot_ proxy n (component from to) unit

data Action = 
       SetYear Int 
     | SetMonth Int 
     | SetDay Int 
     | MakeHistoryRequest Event

type State = 
     { year :: Int, 
       month :: Int, 
       day :: Int, 
       isInit :: Boolean, 
       isLoading :: Boolean,
       yearXs :: Array Int,
       monthXs :: Array Int,
       dayXs :: Array Int
     }

component from to =
  H.mkComponent
    { initialState: 
      const { 
        year: _.year from, 
        month: _.month from, 
        day: _.day from,
        isInit: true,
        isLoading: false,
        yearXs: _.year from .. _.year to,
        monthXs: _.month from .. _.month to,
        dayXs: _.day from .. _.day to

      }
    , render: render from to
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (SetYear idx) = do 
        {yearXs} <- H.get
        for_ (index yearXs idx) \x -> H.modify_ _ { year = x }
      handleAction (SetMonth idx) = do 
        {monthXs} <- H.get
        for_ (index monthXs idx) \x -> H.modify_ _ { month = x }
      handleAction (SetDay idx) = do 
        {dayXs} <- H.get
        for_ (index dayXs idx) \x -> H.modify_ _ { day = x }
      handleAction (MakeHistoryRequest ev) = do 
        H.liftEffect $ preventDefault ev
        H.modify_ _ { isInit = false, isLoading = true }
        {year, month, day} <- H.get
        logDebug $ loc <> "  ---> history request made for a date of " <> 
                   show year <> "-" <> show month <> "-" <> show day
      
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
