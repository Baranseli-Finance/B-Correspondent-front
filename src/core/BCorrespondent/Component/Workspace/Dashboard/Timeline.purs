module BCorrespondent.Component.Workspace.Dashboard.Timeline
  ( Action(..)
  , State
  , applyTimezone
  , initTimeline
  , intToTimePiece
  , populatGaps
  , render
  , setTime
  , slot
  )
  where

import Prelude

import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Component.HTML.Utils (cssSvg, css)

import Halogen as H
import Web.UIEvent.MouseEvent (MouseEvent)
import Data.Maybe (Maybe, fromMaybe, Maybe (..))
import Data.Enum (toEnum, fromEnum, class BoundedEnum)
import Data.Time (setHour, setMinute, Time (..), hour, minute)
import Control.Alt ((<|>))
import Data.Array (reverse, (:), find, uncons, sortBy)
import Data.Lens
import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes.FontSize as Svg
import Halogen.Svg.Attributes as Svg
import Halogen.Svg.Attributes.Color as Svg
import Halogen.HTML.Events (onClick, onMouseMove, onMouseOut, onMouseEnter)
import Halogen.HTML as HH
import Data.Int (toNumber, even, floor)
import Type.Proxy (Proxy(..))

import Undefined

proxy = Proxy :: _ "workspace_dashboard_timeline"

loc = "BCorrespondent.Component.Workspace.Dashboard.Timeline"

slot n {timeline, institution} = HH.slot_ proxy n component {timeline, institution}

type State = 
     { timeline :: Array Back.GapItem,
       institution :: String,
       isBackward :: Boolean,
       isForward :: Boolean,
       timezone :: Int,
       currentGapIdx :: Int 
     }

_timeline = lens _.timeline $ \el x -> el { timeline = x }
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

component =
  H.mkComponent
    { initialState: 
        \{timeline, institution} -> 
           { timeline: timeline,
             institution: institution, 
             isBackward: false,
             isForward: true,
             timezone: 0,
             currentGapIdx: -1
           }
    , render: render
    , eval: H.mkEval H.defaultEval
    }


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

populatGaps timeline xs = 
  timeline <#> \curr@{ start, end} -> 
    let elm = flip find xs \x -> _.start x == start && _.end x == end
    in fromMaybe curr $ flip map elm \{elements, amounts } -> curr # Back._elements .~ elements # Back._amounts .~ amounts

 -- x # Back._start <<< Back._hour %~ ((+) timezone) 
--    # Back._end <<< Back._hour %~ ((+) timezone)
applyTimezone :: Int -> Back.GapItem -> Back.GapItem
applyTimezone _ x = x

mkBackwardButton isBackward = 
  Svg.g 
  [onClick (const Backward)] 
  [
    Svg.text 
    [ cssSvg $ 
      if isBackward 
      then "timeline-travel-button-active" 
      else "timeline-travel-button-blocked",
      Svg.x (toNumber ((1440 / 2) - 50)), 
      Svg.y (toNumber 950), 
      Svg.fill (Svg.Named "black"),
      Svg.fontSize Svg.Large]
    [HH.text "backward"]
  ]

mkForwardButton isForward =
  Svg.g 
  [onClick (const Forward)] 
  [
    Svg.text 
    [ cssSvg 
      if isForward 
      then "timeline-travel-button-active" 
      else "timeline-travel-button-blocked",
      Svg.x (toNumber ((1440 / 2) + 50)), 
      Svg.y (toNumber 950), 
      Svg.fill (Svg.Named "black"),
      Svg.fontSize Svg.Large]
    [HH.text "forward"]
  ]

render {institution, isBackward, isForward, timezone, timeline, currentGapIdx} =
   HH.div [css "timeline-container"]
   [ 
       Svg.svg 
       [Svg.width (toNumber 1440), 
        Svg.height (toNumber 1000)
       ]
       [ 
            Svg.text 
            [Svg.x (toNumber ((1440 / 2))), 
             Svg.y (toNumber 20), 
             Svg.fill (Svg.Named "black"),
             Svg.fontSize Svg.XXLarge
            ]
            [HH.text institution]
       ,    Svg.g [] $ 
              mkBackwardButton isBackward : 
              mkForwardButton isForward :
              renderTimeline (toNumber 10) 0 (map (applyTimezone timezone) timeline) currentGapIdx
       ]
   ]

renderTimeline coordX idx xs currGap = 
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
            Svg.height height,
            if currGap == idx 
            then cssSvg "gap-selected" 
            else cssSvg mempty
          ]
      tmCircle = Svg.circle [Svg.fill (Svg.Named "black"), Svg.cy (height + toNumber 50), Svg.cx coordX, Svg.r (toNumber 5) ]
      lastTmCircle = Svg.circle [Svg.fill (Svg.Named "black"), Svg.cy (height + toNumber 50), Svg.cx (coordX + width), Svg.r (toNumber 5) ]
      mkTm h m shiftX = 
                Svg.text 
                [Svg.x shiftX, 
                Svg.y (height + toNumber 70), 
                Svg.fill (Svg.Named "black")] 
                [HH.text (show (h :: Int) <> ":" <> show (m :: Int))]
      item i h m xs ys = 
        Svg.g [onMouseMove (TotalAmountInGap i xs), onMouseOut (const CancelTotalAmountInGap) ] $ 
          [gap, mkTm h m (coordX - toNumber 10)] <> 
          populateTransactions coordX height width ys <> 
          [tmCircle]
  in case uncons xs of
       Nothing -> []
       Just {head: x, tail: []} -> 
         [Svg.g 
          [onMouseMove (TotalAmountInGap idx (_.amounts x)), onMouseOut (const CancelTotalAmountInGap) ] $
          [gap,
           mkTm 
           ((_.hour <<< _.start) x) 
           ((_.min <<< _.start) x)
           (coordX - toNumber 10),
           mkTm 
           ((_.hour <<< _.end) x) 
           ((_.min <<< _.end) x)
           (coordX + width - toNumber 10) ]] <> 
           populateTransactions coordX height width (x^.Back._elements) <>
           [tmCircle, lastTmCircle]
       Just {head: {amounts, elements, start: {hour, min}}, tail} -> 
         item idx hour min amounts elements : renderTimeline (coordX + width) (idx + 1) tail currGap

populateTransactions x@coordX coordY width xs = 
  go coordY $ flip sortBy xs \x y -> compare (_.tm x :: String) (_.tm y)
  where
    height = toNumber 50
    go y xs =
      case uncons xs of
         Nothing -> []
         Just {head: x, tail: []} -> 
           [mkItem y x]
         Just {head: x, tail} -> 
           mkItem y x : go (y - height) tail
    mkItem y {textualIdent, status: s, ident} = 
      Svg.g 
      [ cssSvg $ 
          if status == Just Back.Pending
          then "timeline-transaction-g-not-allowed" 
          else "timeline-transaction-g", 
        onClick (const (FetchTransaction ident status))
      ] 
      [region y status,
       Svg.text 
       [cssSvg "timeline-transaction-region-item", 
        Svg.x (x + toNumber 3), 
        Svg.y (y + toNumber 25)] 
       [HH.text textualIdent]
      ]
      where status = Back.decodeGapItemUnitStatus s
    region y status =
      Svg.rect
      [ cssSvg "timeline-transaction-region",
        Svg.fill $ chooseColour status,
        Svg.x coordX, 
        Svg.y y, 
        Svg.width width, 
        Svg.height height
      ]
      where
        chooseColour Nothing = Svg.Named "#ffffff"
        chooseColour (Just Back.Pending) = Svg.Named "#ffffff"
        chooseColour (Just Back.Ok) = Svg.Named "#7ddF3a"
        chooseColour (Just Back.Declined) = Svg.Named "#ff5959"