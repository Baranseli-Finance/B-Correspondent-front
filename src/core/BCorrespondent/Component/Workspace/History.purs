module BCorrespondent.Component.Workspace.History (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Workspace.Dashboard.Timeline as Timeline
import BCorrespondent.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (getStore)
import Type.Proxy (Proxy(..))
import Data.Array ((..), index, head, last)
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
     | HandleChild Timeline.Output

type State = 
     { year :: Int, 
       month :: Int, 
       day :: Int, 
       isInit :: Boolean, 
       isLoading :: Boolean,
       loaded :: Boolean,
       yearXs :: Array Int,
       monthXs :: Array Int,
       dayXs :: Array Int,
       error :: Maybe String,
       timeline :: Array Back.GapItem,
       institution :: String
     }

_timeline = lens _.timeline $ \el x -> el { timeline = x }
_isLoading = lens _.isLoading $ \el x -> el { isLoading = x }
_institution = lens _.institution $ \el x -> el { institution = x }
_loaded = lens _.loaded $ \el x -> el { loaded = x }

component from to =
  H.mkComponent
    { initialState: 
      const 
      { year: _.year from, 
        month: _.month from, 
        day: _.day from,
        isInit: true,
        isLoading: false,
        loaded: false,
        yearXs: _.year from .. _.year to,
        monthXs: _.month from .. _.month to,
        dayXs: _.day from .. _.day to,
        error: Nothing,
        timeline: [],
        institution: mempty :: String
      }
    , render: render from to
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction (SetYear idx) = do 
        {yearXs} <- H.get
        for_ (index yearXs idx) \x -> 
          H.modify_ _ { year = x, loaded = false }
      handleAction (SetMonth idx) = do 
        {monthXs} <- H.get
        for_ (index monthXs idx) \x -> 
          H.modify_ _ { month = x, loaded = false }
      handleAction (SetDay idx) = do 
        {dayXs} <- H.get
        for_ (index dayXs idx) \x -> 
          H.modify_ _ { day = x, loaded = false }
      handleAction (MakeHistoryRequest ev) = do 
        H.liftEffect $ preventDefault ev
        {year, month, day, loaded} <- H.get
        logDebug $ loc <> "  ---> history request made for a date of " <> 
                   show year <> "-" <> show month <> "-" <> show day
        if loaded then
          let msg = "timeline for " <> show year <> "-" <> show month <> "-" <> show day <> " already loaded"
          in Async.send $ Async.mkOrdinary msg Async.Info Nothing
        else do
          { config: Config { apiBCorrespondentHost: host }, user } <- getStore
          H.modify_ _ { isInit = false, isLoading = true, error = Nothing }
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
              let newTimeline = 
                    flip Timeline.populatGaps timeline $
                      Timeline.initTimeline 
                      (Timeline.setTime 0 0 time) 
                      (Timeline.setTime 0 1 time)
              H.modify_ \s -> s # _isLoading .~ false # _loaded .~ true # _institution .~ institution # _timeline .~ newTimeline
      handleAction (HandleChild (Timeline.OutputDirection direction timeline)) = do
        logDebug $ loc <> "  ---> HandleChild  " <> show direction <> " " <> show (Back.printTimline timeline)
        let go hour = 
              do
                { config: Config { apiBCorrespondentHost: host }, user } <- getStore
                for_ (user :: Maybe User) \{ token } -> do
                  {year, month, day} <- H.get
                  let params = {year: year, month: month, day: day, direction: Back.encodeDirection direction, hour: hour}
                  resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
                    Back.fetchShiftHistoryTimeline params
                  onFailure resp (Async.send <<< flip Async.mkException loc) 
                    \{success: gaps} -> do
                      time <- H.liftEffect $ nowTime
                      let from = 
                            if direction == Back.Forward 
                            then Timeline.setTime 0 hour time 
                            else Timeline.setTime 0 (hour - 1) time
                      let to = 
                            if direction == Back.Forward 
                            then Timeline.setTime 0 (hour + 1) time 
                            else Timeline.setTime 0 hour time
                      let newTimeline = 
                            flip Timeline.populatGaps gaps $ 
                              Timeline.initTimeline from to
                      let isBackward = 
                            direction == Back.Forward || 
                            (direction == Back.Backward && hour /= 1) 
                      let isForward = 
                            direction == Back.Backward || 
                            (direction == Back.Forward && hour /= 23)
                      H.tell Timeline.proxy 1 $ Timeline.NewTimeline newTimeline isBackward isForward
        case direction of 
          Back.Backward -> for_ (head timeline) \el -> go $ el^.Back._start <<< Back._hour
          Back.Forward -> for_ (last timeline) \el -> go $ el^.Back._end <<< Back._hour
      handleAction (HandleChild (Timeline.OutputUpdate _)) = pure unit
      handleAction (HandleChild (Timeline.OutputTransactionUpdate _)) = pure unit

render from to state = HH.div_ [renderSelectors from to, renderTimline state]

renderSelectors from to =
  HH.div 
  [css "history-selectors"]
  [   
      HH.div_
      [ HH.select [HE.onSelectedIndexChange SetYear, HPExt.style "font-size:20px" ] $
        ((_.year from) .. (_.year to)) <#> \x -> HH.option_ [ HH.text (show x) ]
      ]
  ,   HH.div [HPExt.style "padding-left:10px"] 
      [ HH.select [HE.onSelectedIndexChange SetMonth, HPExt.style "font-size:20px"] $
        ((_.month from) .. (_.month to)) <#> \x -> HH.option_ [ HH.text (show x) ]
      ]
  ,   HH.div [HPExt.style "padding-left:10px"]
      [ HH.select [HE.onSelectedIndexChange SetDay, HPExt.style "font-size:20px" ] $
        ((_.day from) .. (_.day to)) <#> \x -> HH.option_ [ HH.text (show x) ]
      ]
  ,   HH.div [HPExt.style "padding-left:10px"]
      [ 
        HH.form 
        [ HE.onSubmit MakeHistoryRequest ] 
        [ HH.input 
          [ HPExt.type_ HPExt.InputSubmit, 
            HPExt.style "cursor:pointer;font-size:25px", 
            HPExt.value "load"
          ] 
        ]
      ]
  ]

renderTimline {isInit, isLoading, error: Nothing, timeline, institution} =
  if isInit 
  then HH.div [css "history-timeline"] $ textContainer "no timeline"
  else if isLoading
  then HH.div [css "history-timeline"] $ textContainer "loading..."
  else Timeline.slot 1 {timeline: timeline, institution: institution, initShift: Timeline.ShiftInit false true } HandleChild
renderTimline {error: Just val} = HH.div_ [HH.text val]
  
textContainer text = [HH.div [HPExt.style "position:absolute;left:48%;top:40%"] [HH.h3 [HPExt.style "text-transform: uppercase"] [HH.text text]]]