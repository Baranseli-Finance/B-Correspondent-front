module BCorrespondent.Component.Workspace.History (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Workspace.Dashboard.Timeline as T

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (getStore)
import Type.Proxy (Proxy(..))
import Data.Array ((..), index)
import Web.Event.Event (preventDefault, Event)
import Data.Foldable (for_)
import Data.Maybe (Maybe (..))
import Store (User)
import Data.Lens
import Effect.Exception (message)
import System.Time (nowTime)
import Data.Time (hour, minute)

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
       dayXs :: Array Int,
       error :: Maybe String,
       timeline :: T.State
     }

_timeline = lens _.timeline $ \el x -> el { timeline = x }
_isLoading = lens _.isLoading $ \el x -> el { isLoading = x }

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
        dayXs: _.day from .. _.day to,
        error: Nothing,
        timeline: {
          gaps: [],
          institution: mempty :: String, 
          isBackward: false,
          isForward: false,
          timezone: 0,
          currentGapIdx: -1 }
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
        H.modify_ _ { isInit = false, isLoading = true, error = Nothing }
        {year, month, day} <- H.get
        logDebug $ loc <> "  ---> history request made for a date of " <> 
                   show year <> "-" <> show month <> "-" <> show day
        { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        for_ (user :: Maybe User) \{ token } -> do 
          resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
              Back.initHistoryTimeline {year: year, month: month, day: day}
          let failure e = H.modify_ _ { isLoading = false, error = Just $ message e }
          onFailure resp failure \{success: {institution, timeline}} -> do
            let showableTimeline = 
                  flip map (timeline :: Array Back.GapItem) \x -> 
                    x # Back._elements %~ map Back.printGapItemUnit 
                      # Back._amounts %~ map Back.printGapItemAmount
            logDebug $ loc <> " ---> timeline gaps " <> show showableTimeline
            time <- H.liftEffect $ nowTime
            H.modify_ \s ->
               s # _isLoading .~ false
                 # _timeline <<< T._gaps .~
                     flip T.populateTimeline timeline
                       (T.initTimeline (T.setTime 0 0 time) (T.setTime 0 1 time))
                 # _timeline <<< T._institution .~ institution

render from to {isInit, isLoading, error} = HH.div_ [selectors from to, timeline isInit isLoading error]

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

timeline isInit isLoading Nothing = 
  HH.div [css "history-timeline"] $ 
  if isInit 
  then textContainer "no timeline"
  else if isLoading
  then textContainer "loading..."
  else [HH.text "...."]
timeline _ _ (Just error) = HH.text error
  
textContainer text = [HH.div [HPExt.style "position:absolute;left:45%;top:40%"] [HH.h3 [HPExt.style "text-transform: uppercase"] [HH.text text]]]
