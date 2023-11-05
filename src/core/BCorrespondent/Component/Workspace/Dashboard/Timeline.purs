module BCorrespondent.Component.Workspace.Dashboard.Timeline
  ( Action(..)
  , Output(..)
  , Query(..)
  , ShiftInit(..)
  , State
  , applyTimezone
  , initTimeline
  , intToTimePiece
  , populatGaps
  , proxy
  , render
  , setTime
  , slot
  )
  where

import Prelude

import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Component.HTML.Utils (cssSvg, css)
import BCorrespondent.Component.Workspace.Dashboard.Transaction as Dashboard.Transaction
import BCorrespondent.Component.Workspace.Dashboard.Gap as Dashboard.Gap
import BCorrespondent.Capability.LogMessages (logDebug, logError)
import BCorrespondent.Component.Workspace.Dashboard.Timeline.All as Timeline.All 

import Halogen as H
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)
import Data.Maybe (Maybe, fromMaybe, Maybe (..))
import Data.Enum (toEnum, fromEnum, class BoundedEnum)
import Data.Time (setHour, setMinute, Time (..), hour, minute)
import Control.Alt ((<|>))
import Data.Array (reverse, (:), find, uncons, sortBy, head, last)
import Data.Lens
import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes.FontSize as Svg
import Halogen.Svg.Attributes as Svg
import Halogen.Svg.Attributes.Color as Svg
import Halogen.HTML.Events (onClick, onMouseMove, onMouseOut, onMouseEnter)
import Halogen.HTML as HH
import Data.Int (toNumber, even, floor)
import Type.Proxy (Proxy(..))
import Data.Foldable (for_)
import AppM (AppM)
import System.Time (nowTime)

import Undefined

proxy = Proxy :: _ "workspace_dashboard_timeline"

loc = "BCorrespondent.Component.Workspace.Dashboard.Timeline"

data Query a = 
       NewTimeline (Array Back.GapItem) Boolean Boolean a 
     | AddGap a 
     | UpdateTransaction a 
     | WithNewTransaction (Array Back.GapItem) a

data Output = 
       OutputDirection Back.Direction (Array Back.GapItem) 
     | OutputUpdate (Array Back.GapItem) 
     | OutputTransactionUpdate (Array Back.GapItem)

slot n {timeline, institution, initShift} = HH.slot proxy n component {timeline, institution, initShift}

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
_isForward = lens _.isForward $ \el x -> el { isForward = x }
_timezone = lens _.timezone $ \el x -> el { timezone = x }
_currentGapIdx = lens _.currentGapIdx $ \el x -> el { currentGapIdx = x }


data Action =
       Backward
     | Forward
     | FetchTransaction Int Back.GapItemUnitStatus
     | TotalAmountInGap Int (Array Back.GapItemAmount) MouseEvent
     | CancelTotalAmountInGap
     | ShowAllTransactionForParticularGap (Array Back.GapItemUnit)

data ShiftInit = ShiftInit Boolean Boolean

component =
  H.mkComponent
    { initialState: 
        \{timeline, institution, initShift: ShiftInit back forth} -> 
           { timeline: timeline,
             institution: institution,
             isBackward: back,
             isForward: forth,
             timezone: 0,
             currentGapIdx: -1
           }
    , render: render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction
      , handleQuery = handleQuery 
      }
    }
    where
      handleAction Backward = do
        {timeline, isBackward} <- H.get
        when (isBackward) $ H.raise $ OutputDirection Back.Backward timeline
      handleAction Forward = do
        {timeline, isForward} <- H.get
        when (isForward) $ H.raise $ OutputDirection Back.Forward timeline
      handleAction (FetchTransaction  ident status)
        | status == Back.GapItemUnitStatusNotResolved = 
            logError $ loc <> " ---> FetchTransaction, status unknown" 
        | status == Back.Pending = 
            logDebug $ loc <> " ---> FetchTransaction , pending, skip" 
        | otherwise = 
            H.tell Dashboard.Transaction.proxy 1 $ 
              Dashboard.Transaction.Open ident
      handleAction (TotalAmountInGap idx xs ev) = do
        logDebug $ loc <> " ---> TotalAmountInGap, xs: " <> show (xs <#> Back.printGapItemAmount)
        H.tell Dashboard.Gap.proxy 2 $
          Dashboard.Gap.Open 
            { x: Just (clientX ev), 
              y: Just (clientY ev), 
              amounts: xs 
            } 
        H.modify_ _ { currentGapIdx = idx }
      handleAction CancelTotalAmountInGap = do 
        logDebug $ loc <> " CancelTotalAmountInGap"
        H.tell Dashboard.Gap.proxy 2 $
          Dashboard.Gap.Open { x: Nothing, y: Nothing, amounts: [] }
        H.modify_ _ { currentGapIdx = -1 }
      handleAction (ShowAllTransactionForParticularGap xs) = H.tell Timeline.All.proxy 3 $ Timeline.All.Open xs
      handleQuery 
        :: forall a s . Query a
        -> H.HalogenM State Action s Output AppM (Maybe a)
      handleQuery (AddGap a) = do 
        {timeline} <- H.get
        map (const (Just a)) $ H.raise (OutputUpdate timeline)
      handleQuery (NewTimeline newTimeline isBackward isForward a) = do 
        logDebug $ loc <> " ---> NewTimeline " <> show (Back.printTimline newTimeline)
        map (const (Just a)) $ H.modify_ \s ->
          s # _timeline .~ newTimeline # _isBackward .~ isBackward # _isForward .~ isForward
      handleQuery (UpdateTransaction a) = do
        {timeline} <- H.get
        map (const (Just a)) $ H.raise (OutputTransactionUpdate timeline)
      handleQuery (WithNewTransaction newTimeline a) = map (const (Just a)) (H.modify_ \s -> s # _timeline .~ newTimeline)


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


applyTimezone :: Int -> Back.GapItem -> Back.GapItem
applyTimezone _ x = x

canvas = 1420

mkBackwardButton isBackward = 
  Svg.g 
  [onClick (const Backward)] 
  [
    Svg.text 
    [ cssSvg $ 
      if isBackward 
      then "timeline-travel-button-active" 
      else "timeline-travel-button-blocked",
      Svg.x (toNumber (canvas / 2 - 80)), 
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
      Svg.x (toNumber (canvas / 2 + 10)),
      Svg.y (toNumber 950), 
      Svg.fill (Svg.Named "black"),
      Svg.fontSize Svg.Large]
    [HH.text "forward"]
  ]

render {institution, isBackward, isForward, timezone, timeline, currentGapIdx} =
   HH.div [css "timeline-container"]
   [ 
       Svg.svg 
       [Svg.width (toNumber canvas), 
        Svg.height (toNumber 1000)
       ]
       [ 
            Svg.text 
            [Svg.x (toNumber ((canvas / 2 - 80))), 
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
   ,   Dashboard.Transaction.slot 1
   ,   Dashboard.Gap.slot 2
   ,   Timeline.All.slot 3
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
  go 1 coordY xs
  where
    height = toNumber 50
    go c y _ | c == 17 = [mkAllItem y]
    go c y xs =
      case uncons xs of
         Nothing -> []
         Just {head: x, tail: []} -> 
           [mkItem y x]
         Just {head: x, tail} -> 
           mkItem y x : go (c + 1) (y - height) tail
    mkItem y {textualIdent, status: s, ident} = 
      Svg.g 
      [ cssSvg $ 
          if status == Back.Pending
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
    mkAllItem y =
      Svg.g 
      [onClick (const (ShowAllTransactionForParticularGap xs))]
      [ allRegion y,
        Svg.text 
        [cssSvg "timeline-transaction-all-region-item",
         Svg.x (x + width / toNumber 2), 
         Svg.y (y + toNumber 25),
         Svg.dominantBaseline Svg.BaselineMiddle,
         Svg.textAnchor Svg.AnchorMiddle] 
        [HH.text "all"]
      ] 
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
        chooseColour Back.GapItemUnitStatusNotResolved = Svg.Named "#ffffff"
        chooseColour Back.Pending = Svg.Named "#ffffff"
        chooseColour Back.Ok = Svg.Named "#7ddF3a"
        chooseColour Back.Declined = Svg.Named "#ff5959"
    allRegion y = 
     Svg.rect
      [ cssSvg "timeline-transaction-all-region-background",
        Svg.fill $ Svg.Named "#8a8a8a",
        Svg.x coordX, 
        Svg.y y, 
        Svg.width width, 
        Svg.height height
      ]