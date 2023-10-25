module BCorrespondent.Component.Workspace.BalancedBook.History ( Date, slot ) where

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

proxy = Proxy :: _ "workspace_balanced_book_history"

loc = "BCorrespondent.Component.Workspace.BalancedBook.History"

slot n params = HH.slot_ proxy n component params

data Action =
       Initialize
     | HandleChild Timeline.Output

type Date = { year :: Int, month :: Int, day :: Int }

type State = 
     { year :: Int,
       month :: Int,
       day :: Int,
       hour :: Int,
       isInit :: Boolean,
       loaded :: Boolean,
       error :: Maybe String,
       timeline :: Array Back.GapItem,
       institution :: String
     }

_timeline = lens _.timeline $ \el x -> el { timeline = x }
_isLoading = lens _.isLoading $ \el x -> el { isLoading = x }
_institution = lens _.institution $ \el x -> el { institution = x }
_loaded = lens _.loaded $ \el x -> el { loaded = x }

component =
  H.mkComponent
    { initialState: \{year, month, day, hour } ->  
      { year: year,
        month: month,
        day: day,
        hour: hour,
        isInit: true,
        loaded: false,
        error: Nothing,
        timeline: [],
        institution: mempty :: String
      }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction 
      , initialize = pure Initialize 
      }
    }
    where
      handleAction Initialize = do 
        { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        for_ (user :: Maybe User) \{ token } -> do
          {year, month, day, hour} <- H.get
          let params = {year: year, month: month, day: day, direction: Back.encodeDirection Back.Forward, hour: hour}
          resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
            Back.fetchShiftHistoryTimeline params
          onFailure resp (Async.send <<< flip Async.mkException loc) 
            \{success: gaps} -> do
                time <- H.liftEffect $ nowTime
                let from = Timeline.setTime 0 hour time
                let to = Timeline.setTime 0 (hour + 1) time
                let newTimeline = 
                      flip Timeline.populatGaps gaps $ 
                        Timeline.initTimeline from to
                let isBackward = hour /= 0 
                let isForward = hour /= 23

                let showableTimeline = 
                      flip map (newTimeline :: Array Back.GapItem) \x -> 
                        x # Back._elements %~ map Back.printGapItemUnit 
                          # Back._amounts %~ map Back.printGapItemAmount
                logDebug $ loc <> " ---> timeline gaps " <> show showableTimeline
                H.modify_ _ { isInit = false, timeline = newTimeline }
                H.tell Timeline.proxy 0 $ Timeline.NewTimeline newTimeline isBackward isForward
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
                      H.tell Timeline.proxy 0 $ Timeline.NewTimeline newTimeline isBackward isForward
        case direction of 
          Back.Backward -> for_ (head timeline) \el -> go $ el^.Back._start <<< Back._hour
          Back.Forward -> for_ (last timeline) \el -> go $ el^.Back._end <<< Back._hour
      handleAction (HandleChild (Timeline.OutputUpdate _)) = pure unit
      handleAction (HandleChild (Timeline.OutputTransactionUpdate _)) = pure unit

render {isInit, error: Nothing, timeline, institution}
  | isInit = textContainer "loading"
  | otherwise = Timeline.slot 0 {timeline: timeline, institution: institution, initShift: Timeline.ShiftInit false true } HandleChild
render {error: Just val} = HH.div_ [HH.text val]
  
textContainer text = HH.div [css "balanced-book-history-loading"] [HH.h3 [HPExt.style "text-transform: uppercase"] [HH.text text]]