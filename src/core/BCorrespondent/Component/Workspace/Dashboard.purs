module BCorrespondent.Component.Workspace.Dashboard (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (cssSvg)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Async as Async

import Halogen.Store.Monad (getStore)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onClick)
import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes as Svg
import Halogen.Svg.Attributes.Color as Svg
import Type.Proxy (Proxy(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe (..), fromMaybe)
import Effect.Exception (message)
import Data.Int (toNumber, even)
import System.Time (addMinutes, nowTime)
import Data.Time (hour, minute, Time (..), setHour, setMinute)
import Data.Enum (toEnum, fromEnum, class BoundedEnum)
import Data.Time.Component
import Data.Array ((:), sort, length, uncons, head, last, snoc)
import Effect.Aff as Aff
import Store (User)
import Control.Monad.Rec.Class (forever)


import Undefined

proxy = Proxy :: _ "workspace_dashboard"

loc = "BCorrespondent.Component.Workspace.Dashboard"

slot n = HH.slot_ proxy n component unit

data Action = Initialize | Finalize | Update | Backward | Forward

type State = 
     { error :: Maybe String, 
       timeline :: Array Back.GapItem,
       forkId :: Maybe H.ForkId,
       isBackward :: Boolean,
       isForward :: Boolean
     }

component =
  H.mkComponent
    { initialState: const 
      { error: Nothing, 
        timeline: [], 
        forkId: Nothing,
        isBackward: false,
        isForward: false
      }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      , finalize = pure Finalize
      }
    }
    where 
      handleAction Initialize = do
        { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        for_ user \{ token } -> do
          resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
            Back.initUserDashboardDailyBalanceSheet
          let failure e = H.modify_ _ { error = Just $ "cannot load component: " <> message e }
          onFailure resp failure \{ success: {gaps} } -> do
            timeto <- H.liftEffect $ nowTime
            let timetoAdj = setMinute (intToTimePiece (fromEnum (minute timeto) + mod (60 - fromEnum (minute timeto)) 5)) timeto
            timefrom <- H.liftEffect $ addMinutes (-60) timetoAdj
            let timefromAdj = setMinute (intToTimePiece (fromEnum (minute timefrom) + mod (60 - fromEnum (minute timefrom)) 5)) timefrom

            logDebug $ loc <> " ---> timeline start -> end: (" <> show timefromAdj <> ", " <> show timetoAdj <> ")"

            let timeline = initTimeline timefromAdj timetoAdj
            logDebug $ loc <> " ---> init timeline " <> show timeline
        
            forkId <-  H.fork $ forever $ do
               H.liftAff $ Aff.delay $ Aff.Milliseconds 300_000.0
               handleAction Update 

            H.modify_ _ { timeline = populateTimeline timeline gaps }

      handleAction Finalize = map (_.forkId) H.get >>= flip for_ H.kill
      handleAction Update = do
        { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        for_ (user :: Maybe User) \{ token } -> do
          {timeline} <- H.get
          for_ (last timeline) \{end: {hour, min}} -> do
            let start = show hour <> "," <> show min
            let endHour = if min + 5 > 59 then hour + 1 else hour
            let endMin = if min + 5 > 59 then 5 else min + 5
            let end = show endHour <> "," <> show endMin
            resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
              Back.loadNextGap start end
            let failure = Async.send <<< flip Async.mkException loc
            onFailure resp failure \{ success: item } -> do
              logDebug $ loc <> " ---> update timeline from " <> start <> ", to " <> end 
              H.modify_  \s@{timeline: x} -> 
                s { timeline = 
                      fromMaybe [] $ 
                        flip map (uncons x) \{tail} -> 
                          snoc tail item }

      handleAction Backward = logDebug $ loc <> " --->  backward"
      handleAction Forward = logDebug $ loc <> " --->  forward"


initTimeline :: Time -> Time -> Array Back.GapItem
initTimeline from to = 
  if hour from > hour to 
  then go def to [] 
  else go from to []
  where
     def = 
           Time 
           (intToTimePiece 0)
           (intToTimePiece 0)
           (intToTimePiece 0)
           (intToTimePiece 0)
     go from to xs | 
       (hour from == hour to) && 
       (minute from == minute to) = sort xs
     go from to xs =
       let x = 
              { elements: [], 
                start: { 
                  hour: fromEnum $ hour from, 
                  min: fromEnum $ minute from }, 
                end: { 
                  hour: 
                    if fromEnum (minute from) + 5 == 60 
                    then fromEnum $ hour to
                    else fromEnum $ hour from, 
                  min: 
                    if fromEnum (minute from) + 5 == 60 
                    then 0 
                    else fromEnum (minute from) + 5 }
              }
           newFrom = 
             setHour
             (if fromEnum (minute from) + 5 == 60 
             then hour to
             else hour from) $
             flip setMinute def $
              if fromEnum (minute from) + 5 == 60 
              then intToTimePiece 0 
              else intToTimePiece (fromEnum (minute from) + 5)
       in go newFrom to (x:xs)

intToTimePiece :: forall a . BoundedEnum a => Int -> a
intToTimePiece = fromMaybe undefined <<< toEnum

populateTimeline timeline _ = timeline

render {error: Just e} = HH.text e
render {error: Nothing, timeline, isBackward, isForward} =
  Svg.svg 
  [Svg.width (toNumber 1440), 
   Svg.height (toNumber 1000)]
  [ Svg.text 
    [Svg.x (toNumber ((1440 / 2))), 
     Svg.y (toNumber 20), 
     Svg.fill (Svg.Named "black")] 
    [HH.text "Dashboard"]
  , Svg.g [] $ 
      mkBackwardButton isBackward : 
      mkForwardButton isForward : 
      renderTimline (toNumber 10) 0 timeline 
  ]

mkBackwardButton isBackward = 
  Svg.g [onClick (const Backward)] 
  [
    Svg.text 
    [ cssSvg $ 
      if isBackward 
      then "timeline-travel-button-active" 
      else "timeline-travel-button-blocked",
      Svg.x (toNumber ((1440 / 2) - 50)), 
      Svg.y (toNumber 950), 
      Svg.fill (Svg.Named "black")] 
    [HH.text "backward"]
  ]

mkForwardButton isForward =
  Svg.g [onClick (const Forward)] 
  [
    Svg.text 
    [ cssSvg 
      if isForward 
      then "timeline-travel-button-active" 
      else "timeline-travel-button-blocked",
      Svg.x (toNumber ((1440 / 2) + 50)), 
      Svg.y (toNumber 950), 
      Svg.fill (Svg.Named "black")] 
    [HH.text "forward"]
  ]

renderTimline coordX idx xs = 
  let width = toNumber 115
      height = toNumber 850
      coordY = toNumber 50
      color = if even idx then "#e7e4e4" else "#dddada"
      gap = 
             Svg.rect 
             [Svg.fill (Svg.Named color), 
              Svg.x coordX, 
              Svg.y coordY, 
              Svg.width width, 
              Svg.height height
             ]
      tmCircle = Svg.circle [Svg.fill (Svg.Named "black"), Svg.cy (height + toNumber 50), Svg.cx coordX, Svg.r (toNumber 5) ]
      lastTmCircle = Svg.circle [Svg.fill (Svg.Named "black"), Svg.cy (height + toNumber 50), Svg.cx (coordX + width), Svg.r (toNumber 5) ]
      mkTm h m shiftX = 
                Svg.text 
                [Svg.x shiftX, 
                Svg.y (height + toNumber 70), 
                Svg.fill (Svg.Named "black")] 
                [HH.text (show (h :: Int) <> ":" <> show (m :: Int))]
      item h m = Svg.g [] [gap, tmCircle, mkTm h m (coordX - toNumber 10)]
  in case uncons xs of
       Just {head: x, tail: []} -> 
         [Svg.g 
          [] 
          [gap,
           tmCircle, 
           lastTmCircle, 
           mkTm 
           ((_.hour <<< _.start) x) 
           ((_.min <<< _.start) x)
           (coordX - toNumber 10),
           mkTm 
           ((_.hour <<< _.end) x) 
           ((_.min <<< _.end) x)
           (coordX + width - toNumber 10) ]]
       Just {head: {start: {hour, min}}, tail} -> item hour min : renderTimline (coordX + width) (idx + 1) tail
       Nothing -> []