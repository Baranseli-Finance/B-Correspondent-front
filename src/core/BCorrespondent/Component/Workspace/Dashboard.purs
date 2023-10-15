module BCorrespondent.Component.Workspace.Dashboard
  ( slot
  )
  where

import Prelude

import BCorrespondent.Component.HTML.Utils (cssSvg, css)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug, logError)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Component.Async as Async
import BCorrespondent.Component.Workspace.Dashboard.Transaction as Dashboard.Transaction
import BCorrespondent.Component.Workspace.Dashboard.Gap as Dashboard.Gap
import BCorrespondent.Component.Subscription.WS as WS
import BCorrespondent.Component.Subscription.WS.Types

import Halogen.Store.Monad (getStore)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onClick, onMouseMove, onMouseOut, onMouseEnter)
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
import Data.Array ((:), reverse, length, uncons, head, last, snoc, find, concat, sortBy, catMaybes)
import Effect.Aff as Aff
import Store (User)
import Control.Monad.Rec.Class (forever)
import Control.Alt ((<|>))
import Data.Lens
import Data.Ord (compare, abs)
import Web.Socket as WS
import Effect.AVar as Async
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

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
     | UpdateWallet Wallet
     | TotalAmountInGap Int (Array Back.GapItemAmount) MouseEvent
     | CancelTotalAmountInGap

type State = 
     { error :: Maybe String, 
       timeline :: Array Back.GapItem,
       institution :: String,
       forkId :: Maybe H.ForkId,
       isBackward :: Boolean,
       isForward :: Boolean,
       stepsBackward :: Int,
       timezone :: Int,
       wallets :: Array Back.EnumResolvedWallet,
       currentGapIdx :: Int 
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
        timezone: 0,
        wallets: [],
        currentGapIdx: -1
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
          onFailure resp failure \{ success: {dailyBalanceSheet: {institution: title, gaps}, wallets} } -> do
            logDebug $ loc <> " ---> timeline gaps " <> 
              show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) (gaps :: Array Back.GapItem))
            logDebug $ loc <> " ---> timeline institution " <> title
            time <- H.liftEffect $ nowTime
            let adjMin = fromEnum (minute time) + mod (60 - fromEnum (minute time)) 5
            let roundMin = if adjMin == 60 then 0 else adjMin
            let from = setH (if roundMin == 0 then fromEnum (hour time) else fromEnum (hour time) - 1) $ setMin roundMin time
            let to = setH (if roundMin == 0 then fromEnum (hour time) + 1 else fromEnum (hour time)) $ setMin roundMin time

            logDebug $ loc <> " ---> timeline start -> end: (" <> show from <> ", " <> show to <> ")"

            let timeline = 
                  flip populateTimeline gaps $ 
                    initTimeline from to
            logDebug $ loc <> " ---> init timeline " <> 
              show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) timeline)
        
            forkId <-  H.fork $ do
               logDebug $ loc <> " ---> timeline updater has been activated"
               forever $ delay *> handleAction Update

            timezone <- H.liftEffect getTimezone

            WS.subscribe loc WS.transactionUrl (Just (WS.encodeResource WS.Transaction)) $ 
              handleAction <<< UpdateTransaction <<< _.success

            WS.subscribe loc WS.walletUrl (Just (WS.encodeResource WS.Wallet)) $
              handleAction <<< UpdateWallet <<< _.success

            H.modify_ _ 
              { timeline = timeline,
                institution = title, 
                isBackward = 
                  if fromEnum (hour time) == 0 
                  then false 
                  else true,
                forkId = Just forkId,
                timezone = timezone,
                wallets = 
                  flip sortBy (map resolveEnums wallets) \x y -> 
                    compare (_.walletType x) (_.walletType y) <> 
                    compare (_.currency x) (_.currency y)
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
                logDebug $ loc <> " --->  backward, gaps " <> 
                  show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) (gaps :: Array Back.GapItem))
                time <- H.liftEffect $ nowTime
                let newStartPoint | hour - 1 < 0 = setTime min 23 time 
                                  | otherwise = setTime min (hour - 1) time
                let newEndPoint = setTime min hour time

                logDebug $ loc <> " --->  backward. new points " <> show newStartPoint <> ", " <> show newEndPoint
                 
                let newTimeline = 
                      flip populateTimeline gaps $ 
                        initTimeline newStartPoint newEndPoint
                logDebug $ loc <> " --->  backward. current timline " <> 
                  show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) newTimeline)

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
              -- logDebug $ loc <> " --->  forward. points " <> show ps
              time <- H.liftEffect $ nowTime
              let minutes = fromEnum $ minute time

              logDebug $ loc <> " --->  forward. end point " <> show p
              logDebug $ loc <> " --->  forward. current point {" <> show (Time.hour time) <> ", " <> show (minute time) <> "}"
              logDebug $ loc <> " --->  forward. current shift backwards " <> show stepsBackward 

              let diffMin = 
                    ((hour + stepsBackward) * 60 + min) -
                    (fromEnum (Time.hour time) * 60 + minutes)

              logDebug $ loc <> " --->  forward, diff " <> show diffMin

              let gap | 0 <= diffMin && diffMin < 5 = 0
                      | 0 > diffMin && diffMin > -60 = abs diffMin + mod (60 - abs diffMin) 5
                      | otherwise = floor $ toNumber (abs diffMin / 60)

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
                        logDebug $ loc <> " --->  forward. current timline " <> 
                          show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) newTimeline) 
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
                        let minutesAdj = minutes + mod (60 - minutes) 5
                        doNoGap (setTime newMin newHour time) (setMinute (intToTimePiece minutesAdj) time) point
              let -- in this case just add an hour and then two hours to the end point
                  -- example: supposing the initial end point were 9.45, current time were 11.50
                  -- we have a gap of 2 hour and 5 min, 
                  -- new start point: 10.45 (9.45 + 1 hour), end point 10.45 (9.45 + 2 hours)
                  gapMoreThen2Hour =
                    let msg = "gapMoreThen2Hour hasn't been implemented yet. reload page"
                    in Async.send $ Async.mkOrdinary msg Async.Debug (Just loc) 

              let doWithGap | 0 > diffMin && diffMin > -60 = gapLessThenHour
                            | otherwise = gapMoreThen2Hour

              if 0 <= diffMin && diffMin < 5
              then
                let point = show hour <> "," <> show min
                in doNoGap (setTime min hour time) (setTime min (hour + 1) time) point
              else doWithGap       
      handleAction (FetchTransaction ident status)
        | status == Nothing = logError $ loc <> " ---> FetchTransaction, status unknown" 
        | status == Just Back.Pending = logDebug $ loc <> " ---> FetchTransaction , pending, skip" 
        | otherwise = H.tell Dashboard.Transaction.proxy 1 $ Dashboard.Transaction.Open ident
     
      handleAction (UpdateTransaction {hour, min, textualIdent: ident, status: newStatus}) = do
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

      handleAction (UpdateWallet wallet@{ident, amount: new}) = do
        logDebug $ loc <> " wallet update --- >" <> show wallet
        {wallets: old} <- H.get
        H.modify_ _ { wallets = flip map old \x -> if _.ident x == ident then x { amount = new } else x }

      handleAction (TotalAmountInGap idx xs ev) = do
        H.tell Dashboard.Gap.proxy 2 $ 
          Dashboard.Gap.Open { x: Just (clientX ev), y: Just (clientY ev), amounts: xs } 
        H.modify_ _ { currentGapIdx = idx }
        
      handleAction CancelTotalAmountInGap = do 
        logDebug $ loc <> " CancelTotalAmountInGap"
        H.tell Dashboard.Gap.proxy 2 $
          Dashboard.Gap.Open { x: Nothing, y: Nothing, amounts: [] }
        H.modify_ _ { currentGapIdx = -1 }


      handleAction Finalize = do
        map (_.forkId) H.get >>= flip for_ H.kill
        { wsVar } <- getStore
        wsm <- H.liftEffect $ Async.tryTake wsVar
        for_ wsm \xs ->
          for_ xs \{ ws, forkId } -> do
            H.kill forkId
            H.liftEffect $ WS.close ws
            logDebug $ loc <> " ---> ws has been killed"


resolveEnums :: Back.Wallet -> Back.EnumResolvedWallet
resolveEnums x = 
  x { walletType = 
       fromMaybe Back.WalletTypeNotResolved $
         Back.decodeWalletType (_.walletType x),
     currency =
        fromMaybe Back.CurrencyNotResolved $
         Back.decodeCurrency (_.currency x) 
    }

setTime m h = 
  setMinute (fromMaybe undefined (toEnum m <|> toEnum 0)) <<< 
  setHour (fromMaybe undefined (toEnum h <|> toEnum 0))

setMin m = setMinute (fromMaybe undefined (toEnum m <|> toEnum 0))

setH h = setHour (fromMaybe undefined (toEnum h <|> toEnum 0))

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

intToTimePiece :: forall a . BoundedEnum a => Int -> a
intToTimePiece = fromMaybe bottom <<< toEnum

populateTimeline timeline xs = 
  timeline <#> \curr@{ start, end} -> 
    let elm = flip find xs \x -> _.start x == start && _.end x == end
    in fromMaybe curr $ flip map elm \{elements, amounts } -> curr # Back._elements .~ elements # Back._amounts .~ amounts

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
      Svg.fill (Svg.Named "black"),
      Svg.fontSize Svg.Large]
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
      Svg.fill (Svg.Named "black"),
      Svg.fontSize Svg.Large]
    [HH.text "forward"]
  ]

renderTimline coordX idx xs currGap = 
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
         item idx hour min amounts elements : renderTimline (coordX + width) (idx + 1) tail currGap

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
      let chooseColour Nothing = Svg.Named "#ffffff"
          chooseColour (Just Back.Pending) = Svg.Named "#ffffff"
          chooseColour (Just Back.Ok) = Svg.Named "#7ddF3a"
          chooseColour (Just Back.Declined) = Svg.Named "#ff5959"
      in
        Svg.rect
        [ cssSvg "timeline-transaction-region",
          Svg.fill $ chooseColour status,
          Svg.x coordX, 
          Svg.y y, 
          Svg.width width, 
          Svg.height height]

renderWallets xs = 
  [  HH.div [css "wallet"] (catMaybes (map (go ((==) Back.Debit)) xs))
  ,  HH.div [css "wallet"] (catMaybes (map (go ((==) Back.Credit)) xs))
  ]
  where 
    go cond {walletType, currency, amount} 
      | cond walletType = 
          Just $ 
            HH.div_
            [HH.span [css "wallet-text"] [HH.text (show walletType <> "(" <> show currency <> "): ")], 
             HH.span [css "wallet-amount"] [HH.text (show amount)]
            ]
      | otherwise = Nothing

render {error: Just e} = HH.text e
render st@{ error: Nothing } =
  HH.div_ 
  [
      HH.div [css "wallet-container"] $ renderWallets (_.wallets st)
  ,   HH.div [css "timeline-container"]
      [ Svg.svg 
        [Svg.width (toNumber 1440), 
         Svg.height (toNumber 1000)]
        [ Svg.text 
          [Svg.x (toNumber ((1440 / 2))), 
           Svg.y (toNumber 20), 
           Svg.fill (Svg.Named "black"),
           Svg.fontSize Svg.XXLarge]
          [HH.text (_.institution st)]
        , Svg.g [] $ 
            mkBackwardButton (_.isBackward st) : 
            mkForwardButton (_.isForward st) :
            renderTimline (toNumber 10) 0 (map (applyTimezone (_.timezone st)) (_.timeline st)) (_.currentGapIdx st)
        ]
      ]
  ,   Dashboard.Transaction.slot 1
  ,   Dashboard.Gap.slot 2
  ]