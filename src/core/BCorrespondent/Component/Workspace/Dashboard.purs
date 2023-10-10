module BCorrespondent.Component.Workspace.Dashboard (slot) where

import Prelude

import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)

import Halogen.Store.Monad (getStore)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
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
import Data.Array ((:), sort, length, tail)


import Undefined

proxy = Proxy :: _ "workspace_dashboard"

loc = "BCorrespondent.Component.Workspace.Dashboard"

slot n = HH.slot_ proxy n component unit

data Action = Initialize

type State = 
     { error :: Maybe String, 
       timeline :: Array Back.GapItem 
     }

component =
  H.mkComponent
    { initialState: const { error: Nothing, timeline: [] }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
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

            H.modify_ _ { timeline = populateTimeline timeline gaps }


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
render {error: Nothing, timeline} =
  Svg.svg 
  [Svg.width (toNumber 1380), 
   Svg.height (toNumber 900)]
  [ Svg.text 
    [Svg.x (toNumber ((1380 / 2) - 100)), 
     Svg.y (toNumber 20), 
     Svg.fill (Svg.Named "black")] 
    [HH.text "dashboard"]
  , Svg.g [] $ renderTimline (toNumber 0) 0 timeline  
  ]

renderTimline coordX idx xs = 
  let width = toNumber 115
      height = toNumber 850
      coordY = toNumber 50
      color = if even idx then "#E7E4E4" else "#DDDADA"
      item = 
             Svg.rect 
             [Svg.fill (Svg.Named color), 
              Svg.x coordX, 
              Svg.y coordY, 
              Svg.width width, 
              Svg.height height 
             ]
  in case tail xs of
       Just [_] -> [item]
       Just ys -> item : renderTimline (coordX + width) (idx + 1) ys
       Nothing -> []