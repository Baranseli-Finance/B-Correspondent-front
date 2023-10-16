module BCorrespondent.Component.Workspace.Dashboard.Timeline
  ( Action(..)
  , State
  , _currentGapIdx
  , _gaps
  , _institution
  , _isBackward
  , _isForwarc
  , _timezone
  , applyTimezone
  , initTimeline
  , intToTimePiece
  , populateTimeline
  , setTime
  )
  where

import Prelude

import BCorrespondent.Api.Foreign.Back as Back

import Web.UIEvent.MouseEvent (MouseEvent)
import Data.Maybe (Maybe, fromMaybe)
import Data.Enum (toEnum, fromEnum, class BoundedEnum)
import Data.Time (setHour, setMinute, Time (..), hour, minute)
import Control.Alt ((<|>))
import Data.Array (reverse, (:), find)
import Data.Lens

import Undefined

type State = 
     { gaps :: Array Back.GapItem,
       institution :: String,
       isBackward :: Boolean,
       isForward :: Boolean,
       timezone :: Int,
       currentGapIdx :: Int 
     }

_gaps = lens _.gaps $ \el x -> el { gaps = x }
_institution = lens _.institution $ \el x -> el { institution = x }
_isBackward = lens _.isBackward $ \el x -> el { isBackward = x }
_isForwarc = lens _.isForward $ \el x -> el { isForward = x }
_timezone = lens _.timezone $ \el x -> el { timezone = x }
_currentGapIdx = lens _.currentGapIdx $ \el x -> el { currentGapIdx = x }



data Action =
       Backward
     | Forward
     | FetchTransaction Int (Maybe Back.GapItemUnitStatus)
     | TotalAmountInGap Int (Array Back.GapItemAmount) MouseEvent
     | CancelTotalAmountInGap

intToTimePiece :: forall a . BoundedEnum a => Int -> a
intToTimePiece = fromMaybe bottom <<< toEnum

setTime m h = setMinute (fromMaybe undefined (toEnum m <|> toEnum 0)) <<< setHour (fromMaybe undefined (toEnum h <|> toEnum 0))

initTimeline :: Time -> Time -> Array Back.GapItem
initTimeline from to = go from to []
  where
     def = 
           Time 
           (intToTimePiece 0)
           (intToTimePiece 0)
           (intToTimePiece 0)
           (intToTimePiece 0)
     go from to xs | 
       (hour from == hour to) && 
       (minute from == minute to) = reverse xs
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
                    else fromEnum (minute from) + 5 },
                amounts: []   
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

populateTimeline timeline xs = 
  timeline <#> \curr@{ start, end} -> 
    let elm = flip find xs \x -> _.start x == start && _.end x == end
    in fromMaybe curr $ flip map elm \{elements, amounts } -> curr # Back._elements .~ elements # Back._amounts .~ amounts

 -- x # Back._start <<< Back._hour %~ ((+) timezone) 
--    # Back._end <<< Back._hour %~ ((+) timezone)
applyTimezone :: Int -> Back.GapItem -> Back.GapItem
applyTimezone _ x = x