module BCorrespondent.Component.Workspace.Dashboard (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (cssSvg)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug, logError)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Async as Async
import BCorrespondent.Component.Workspace.Dashboard.Transaction as Dashboard.Transaction
import BCorrespondent.Component.Subscription.WS as WS
import BCorrespondent.Component.Subscription.WS.Types

import Halogen.Store.Monad (getStore)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onClick)
import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes.FontSize as Svg
import Halogen.Svg.Attributes as Svg
import Halogen.Svg.Attributes.Color as Svg
import Type.Proxy (Proxy(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe (..), fromMaybe)
import Effect.Exception (message)
import Data.Int (toNumber, even, floor)
import System.Time (addMinutes, nowTime, getTimezone)
import Data.Time (hour, minute, Time (..), setHour, setMinute)
import Data.Time as Time
import Data.Enum (toEnum, fromEnum, class BoundedEnum)
import Data.Time.Component
import Data.Array ((:), reverse, length, uncons, head, last, snoc, find, concat, sortBy)
import Effect.Aff as Aff
import Store (User)
import Control.Monad.Rec.Class (forever)
import Control.Alt ((<|>))
import Data.Lens
import Data.Ord (compare)
import Web.Socket as WS
import Effect.AVar as Async

import Undefined

proxy = Proxy :: _ "workspace_dashboard"

loc = "BCorrespondent.Component.Workspace.Dashboard"

slot n = HH.slot_ proxy n component unit

data Action = 
       Initialize 
     | Finalize 
     | Update 
     | Backward 
     | Forward
     | FetchTransaction Int (Maybe Back.GapItemUnitStatus)
     | UpdateTransaction Transaction

type State = 
     { error :: Maybe String, 
       timeline :: Array Back.GapItem,
       institution :: String,
       forkId :: Maybe H.ForkId,
       isBackward :: Boolean,
       isForward :: Boolean,
       stepsBackward :: Int,
       timezone :: Int
     }

component =
  H.mkComponent
    { initialState: const 
      { error: Nothing, 
        timeline: [],
        institution: mempty, 
        forkId: Nothing,
        isBackward: false,
        isForward: false,
        stepsBackward: 0,
        timezone: 0
      }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      , finalize = pure Finalize
      }
    }
    where
      delay = H.liftAff $ Aff.delay $ Aff.Milliseconds 300_000.0
      handleAction Initialize = do
        { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        for_ user \{ token } -> do
          resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ Back.initDashboard
          let failure e = H.modify_ _ { error = Just $ "cannot load component: " <> message e }
          onFailure resp failure \{ success: {dailyBalanceSheet: {institution: title, gaps}} } -> do
            logDebug $ loc <> " ---> timeline gaps " <> show (gaps :: Array Back.GapItem)
            logDebug $ loc <> " ---> timeline institution " <> title
            timeto <- H.liftEffect $ nowTime
            let timetoAdj = setMinute (intToTimePiece (fromEnum (minute timeto) + mod (60 - fromEnum (minute timeto)) 5)) timeto
            timefrom <- H.liftEffect $ addMinutes (-60) timetoAdj
            let timefromAdj = setMinute (intToTimePiece (fromEnum (minute timefrom) + mod (60 - fromEnum (minute timefrom)) 5)) timefrom

            logDebug $ loc <> " ---> timeline start -> end: (" <> show timefromAdj <> ", " <> show timetoAdj <> ")"

            let timeline = 
                  flip populateTimeline gaps $ 
                    initTimeline timefromAdj timetoAdj
            logDebug $ loc <> " ---> init timeline " <> show timeline
        
            forkId <-  H.fork $ do
               logDebug $ loc <> " ---> timeline updater has been activated"
               forever $ delay *> handleAction Update

            timezone <- H.liftEffect getTimezone

            WS.subscribe loc WS.dashboardUrl (Just (WS.showResource WS.Transaction)) 
             \{success: new} -> handleAction $ UpdateTransaction new

            H.modify_ _ 
              { timeline = timeline,
                institution = title, 
                isBackward = 
                  if fromEnum (hour timeto) == 0 
                  then false 
                  else true,
                forkId = Just forkId,
                timezone = timezone
              }

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
            onFailure resp failure \{ success: {gap} } -> do
              logDebug $ loc <> " ---> update timeline from " <> start <> ", to " <> end 
              H.modify_  \s@{timeline: x} -> s { timeline = fromMaybe [] $ flip map (uncons x) \{tail} -> snoc tail gap }

      handleAction Backward = do
        {isBackward} <- H.get
        when isBackward $ do
          logDebug $ loc <> " --->  backward"
          { config: Config { apiBCorrespondentHost: host }, user } <- getStore
          for_ (user :: Maybe User) \{ token } -> do
            {timeline, stepsBackward} <- H.get
            for_ (uncons timeline) \{head: {start: p@{hour, min}}} -> do
              let point = show hour <> "," <> show min
              resp <- Request.makeAuth (Just token) host Back.mkFrontApi $
                Back.fetchTimelineForParticularHour Back.Backward point
              let failure = Async.send <<< flip Async.mkException loc  
              onFailure resp failure \{success: gaps} -> do
                map (_.forkId) H.get >>= flip for_ H.kill
                logDebug $ loc <> " --->  backward, gaps " <> show (gaps :: Array Back.GapItem)
                time <- H.liftEffect $ nowTime
                let newStartPoint | hour - 1 < 0 = setTime min 23 time 
                                  | otherwise = setTime min (hour - 1) time
                let newEndPoint = setTime min hour time

                logDebug $ loc <> " --->  backward. new points " <> show newStartPoint <> ", " <> show newEndPoint
                 
                let newTimeline = 
                      flip populateTimeline gaps $ 
                        initTimeline newStartPoint newEndPoint
                logDebug $ loc <> " --->  backward. current timline " <> show newTimeline 

                H.modify_ _ 
                  { timeline = newTimeline,
                    isBackward = if hour - 1 < 0 then false else true,
                    isForward = true,
                    forkId = Nothing,
                    stepsBackward = stepsBackward + 1
                  }
      handleAction Forward = do
        {stepsBackward} <- H.get
        when (stepsBackward /= 0) $ do 
          logDebug $  loc <> " ---> forward"
          { config: Config { apiBCorrespondentHost: host }, user } <- getStore
          for_ (user :: Maybe User) \{ token } -> do
            {timeline, stepsBackward} <- H.get
            for_ (last timeline) \ps@{end: p@{hour, min}} -> do
              logDebug $ loc <> " --->  forward. points " <> show ps
              time <- H.liftEffect $ nowTime
              let minutes = fromEnum $ minute time
              let minutesAdj = if minutes > 55 then 0 else minutes + mod (60 - minutes) 5

              logDebug $ loc <> " --->  forward. end point " <> show p
              logDebug $ loc <> " --->  forward. current point {" <> show (Time.hour time) <> ", " <> show minutesAdj <> "}"
              logDebug $ loc <> " --->  forward. current shift backwards " <> show stepsBackward 

              let diffMin = 
                    fromEnum (Time.hour time) * 60 + 
                    (minutes + mod (60 - minutes) 5) - 
                    ((hour + stepsBackward) * 60 + min)

              logDebug $ loc <> " --->  forward, diff " <> show diffMin

              let gap | diffMin == 0 || diffMin == 60 = 0
                      | diffMin < 60 = diffMin
                      | otherwise = floor $ toNumber (diffMin / 60)

              logDebug $ loc <> " --->  forward. gap " <> show gap

              let doNoGap from to point = do
                    logDebug $ loc <> " --->  forward, doNoGap from -> to " <> show from <> ", " <> show to
                    logDebug $ loc <> " --->  forward. doNoGap, point " <> show point
                    { config: Config { apiBCorrespondentHost: host }, user } <- getStore
                    for_ (user :: Maybe User) \{ token } -> do
                      resp <- Request.makeAuth (Just token) host Back.mkFrontApi $
                        Back.fetchTimelineForParticularHour Back.Forward point
                      let failure = Async.send <<< flip Async.mkException loc  
                      onFailure resp failure \{success: gaps} -> do
                        
                        let checkForward = if stepsBackward - 1 == 0 then false else true

                        forkId <- 
                          if not checkForward
                          then map Just $ H.fork $ do
                                logDebug $ loc <> " ---> timeline updater has been activated"
                                forever $ delay *> handleAction Update
                         else pure Nothing

                        let newTimeline = 
                              flip populateTimeline gaps $ 
                                initTimeline from to
                        logDebug $ loc <> " --->  forward. current timline " <> show newTimeline     
                        H.modify_ _ 
                          { timeline = newTimeline,
                            isForward = checkForward,
                            forkId = forkId,
                            stepsBackward = stepsBackward - 1,
                            isBackward = true
                          }

              let addGapTomin s g = if s + g > 60 then 0 else s + g
              let addGapToHour adjMin h = if adjMin == 0 then h + 1 else h 
              let -- if the gap is less then an hour we just need to adjust end point by thereof
                  -- example: supposing the initial end point were 9.45, current time were 10.55
                  -- the gap is 10 min
                  -- new start point: 9.55 (9.45 + 10 min), end point 10.55
                  gapLessThenHour 
                    | stepsBackward > 1 = 
                        let point = show hour <> "," <> show min
                        in doNoGap ((setTime min hour) time) (setTime min (hour + 1) time) point
                    | otherwise = do            
                        logDebug $ loc <> " --->  forward, gapLessThenHour"
                        let newMin = addGapTomin min gap
                        let newHour = addGapToHour newMin hour
                        let point = show newHour <> "," <> show newMin
                        doNoGap (setTime newMin newHour time) (setMinute (intToTimePiece minutesAdj) time) point
              let -- in this case just add an hour and then two hours to the end point
                  -- example: supposing the initial end point were 9.45, current time were 11.50
                  -- we have a gap of 2 hour and 5 min, 
                  -- new start point: 10.45 (9.45 + 1 hour), end point 10.45 (9.45 + 2 hours)
                  gapMoreThen2Hour =
                    let msg = "gapMoreThen2Hour hasn't been implemented yet. reload page"
                    in Async.send $ Async.mkOrdinary msg Async.Debug (Just loc) 

              let doWithGap | gap < 60 = gapLessThenHour
                            | otherwise = gapMoreThen2Hour

              if gap == 0 
              then
                let point = show hour <> "," <> show min
                in doNoGap (setTime min hour time) (setTime min (hour + 1) time) point
              else doWithGap       
      handleAction (FetchTransaction ident status)
        | status == Nothing = logError $ loc <> " ---> FetchTransaction, status unknown" 
        | status == Just Back.Pending = logDebug $ loc <> " ---> FetchTransaction , pending, skip" 
        | otherwise = H.tell Dashboard.Transaction.proxy 1 $ Dashboard.Transaction.Open ident
     
      handleAction (UpdateTransaction tr@{hour, min, textualIdent: ident, status: newStatus}) = do 
        logDebug $ loc <> " ---> update transaction " <> show tr
        {timeline} <- H.get
        let newTimeline = 
              flip map timeline \x ->
                if x^.Back._start <<< Back._hour * 60 + 
                   x^.Back._start <<< Back._min <= 
                   hour * 60 + min
                   && x^.Back._end <<< Back._hour * 
                      60 + x^.Back._end <<< Back._min >= 
                   hour * 60 + min
                then x # Back._elements %~ \xs -> 
                       flip map xs \el@{textualIdent} -> 
                         if ident ==  textualIdent 
                         then el { status = newStatus } 
                         else el
                else x
        H.modify_ _ { timeline = newTimeline }

      handleAction Finalize = do
        map (_.forkId) H.get >>= flip for_ H.kill
        { wsVar } <- getStore
        wsm <- H.liftEffect $ Async.tryTake wsVar
        for_ wsm \xs ->
          for_ xs \{ ws, forkId } -> do
            H.kill forkId
            H.liftEffect $ WS.close ws
            logDebug $ loc <> " ---> ws has been killed"


setTime m h = 
  setMinute (fromMaybe undefined (toEnum m <|> toEnum 0)) <<< 
  setHour (fromMaybe undefined (toEnum h <|> toEnum 0))

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
intToTimePiece = fromMaybe bottom <<< toEnum

populateTimeline timeline xs = 
  timeline <#> \curr@{ start, end} -> 
    let elm = flip find xs \x -> _.start x == start && _.end x == end
    in fromMaybe curr $ flip map elm \{elements } -> curr # Back._elements .~ elements

 -- x # Back._start <<< Back._hour %~ ((+) timezone) 
--    # Back._end <<< Back._hour %~ ((+) timezone)
applyTimezone :: Int -> Back.GapItem -> Back.GapItem
applyTimezone _ x = x

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
      item h m xs = 
        Svg.g [] $ 
          [gap, mkTm h m (coordX - toNumber 10)] <> 
          populateTransactions coordX height width xs <> 
          [tmCircle]
  in case uncons xs of
       Nothing -> []
       Just {head: x, tail: []} -> 
         [Svg.g 
          [] $
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
       Just {head: {elements, start: {hour, min}}, tail} -> 
         item hour min elements : renderTimline (coordX + width) (idx + 1) tail

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
      where status = Back.readGapItemUnitStatus s
    region y status =
      let chooseColour Nothing = Svg.Named "#ffffff"
          chooseColour (Just Back.Pending) = Svg.Named "#ffffff"
          chooseColour (Just Back.ProcessedOk) = Svg.Named "#7ddF3a"
          chooseColour (Just Back.ProcessedDecline) = Svg.Named "#ff5959"
      in
        Svg.rect
        [ cssSvg "timeline-transaction-region",
          Svg.fill $ chooseColour status,
          Svg.x coordX, 
          Svg.y y, 
          Svg.width width, 
          Svg.height height]

render {error: Just e} = HH.text e
render {error: Nothing, timeline, isBackward, isForward, timezone, institution} =
  HH.div_ 
  [
      Svg.svg 
      [Svg.width (toNumber 1440), 
      Svg.height (toNumber 1000)]
      [ Svg.text 
        [Svg.x (toNumber ((1440 / 2))), 
        Svg.y (toNumber 20), 
        Svg.fill (Svg.Named "black"),
        Svg.fontSize Svg.XXLarge]
        [HH.text institution]
      , Svg.g [] $ 
          mkBackwardButton isBackward : 
          mkForwardButton isForward :
          renderTimline (toNumber 10) 0 (map (applyTimezone timezone) timeline)
      ]
  ,   Dashboard.Transaction.slot 1
  ]