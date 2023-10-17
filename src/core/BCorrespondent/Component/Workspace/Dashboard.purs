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
import BCorrespondent.Component.Workspace.Dashboard.Timeline as Timeline

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
     | UpdateTransaction Transaction
     | UpdateWallet Wallet
     | HandleChild Timeline.Output
    
type State = 
     { error :: Maybe String,
       forkId :: Maybe H.ForkId,
       stepsBackward :: Int,
       wallets :: Array Back.EnumResolvedWallet,
       timeline :: Array Back.GapItem,
       institution :: String
     }

component =
  H.mkComponent
    { initialState: const 
      { error: Nothing,
        forkId: Nothing,
        stepsBackward: 0,
        wallets: [],
        timeline: [],
        institution: mempty
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
            let setH h = setHour (fromMaybe undefined (toEnum h <|> toEnum 0))
            let setMin m = setMinute (fromMaybe undefined (toEnum m <|> toEnum 0))
            let from = setH (if roundMin == 0 then fromEnum (hour time) else fromEnum (hour time) - 1) $ setMin roundMin time
            let to = setH (if roundMin == 0 then fromEnum (hour time) + 1 else fromEnum (hour time)) $ setMin roundMin time

            logDebug $ loc <> " ---> timeline start -> end: (" <> show from <> ", " <> show to <> ")"

            let timeline = 
                  flip Timeline.populatGaps gaps $ 
                    Timeline.initTimeline from to
            logDebug $ loc <> " ---> init timeline " <> 
              show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) timeline)
        
            forkId <-  H.fork $ do
               logDebug $ loc <> " ---> timeline updater has been activated"
               forever $ delay *> handleAction Update

            WS.subscribe loc WS.transactionUrl (Just (WS.encodeResource WS.Transaction)) $ 
              handleAction <<< UpdateTransaction <<< _.success

            WS.subscribe loc WS.walletUrl (Just (WS.encodeResource WS.Wallet)) $
              handleAction <<< UpdateWallet <<< _.success

            H.modify_ _ 
              { forkId = Just forkId,
                wallets = 
                  flip sortBy (map resolveEnums wallets) \x y -> 
                    compare (_.walletType x) (_.walletType y) <> 
                    compare (_.currency x) (_.currency y),
                timeline = timeline,
                institution = title
              }

      handleAction Update = undefined
        -- { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        -- for_ (user :: Maybe User) \{ token } -> do
        --   {timeline} <- H.get
        --   for_ (last timeline) \{end: {hour, min}} -> do
        --     let start = show hour <> "," <> show min
        --     let endHour = if min + 5 > 59 then hour + 1 else hour
        --     let endMin = if min + 5 > 59 then 5 else min + 5
        --     let end = show endHour <> "," <> show endMin
        --     resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
        --       Back.loadNextGap start end
        --     let failure = Async.send <<< flip Async.mkException loc
        --     onFailure resp failure \{ success: {gap} } -> do
        --       logDebug $ loc <> " ---> update timeline from " <> start <> ", to " <> end 
        --       H.modify_  \s@{timeline: x} -> s { timeline = fromMaybe [] $ flip map (uncons x) \{tail} -> snoc tail gap }

      -- handleAction Backward = undefined
        -- {isBackward} <- H.get
        -- when isBackward $ do
        --   logDebug $ loc <> " --->  backward"
        --   { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        --   for_ (user :: Maybe User) \{ token } -> do
        --     {timeline, stepsBackward} <- H.get
        --     for_ (uncons timeline) \{head: {start: p@{hour, min}}} -> do
        --       let point = show hour <> "," <> show min
        --       resp <- Request.makeAuth (Just token) host Back.mkFrontApi $
        --         Back.fetchTimelineForParticularHour Back.Backward point
        --       let failure = Async.send <<< flip Async.mkException loc  
        --       onFailure resp failure \{success: gaps} -> do
        --         map (_.forkId) H.get >>= flip for_ H.kill
        --         logDebug $ loc <> " --->  backward, gaps " <> 
        --           show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) (gaps :: Array Back.GapItem))
        --         time <- H.liftEffect $ nowTime
        --         let newStartPoint | hour - 1 < 0 = setTime min 23 time 
        --                           | otherwise = setTime min (hour - 1) time
        --         let newEndPoint = setTime min hour time

        --         logDebug $ loc <> " --->  backward. new points " <> show newStartPoint <> ", " <> show newEndPoint
                 
        --         let newTimeline = 
        --               flip populatGaps gaps $ 
        --                 initTimeline newStartPoint newEndPoint
        --         logDebug $ loc <> " --->  backward. current timline " <> 
        --           show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) newTimeline)

        --         H.modify_ _ 
        --           { timeline = newTimeline,
        --             isBackward = if hour - 1 < 0 then false else true,
        --             isForward = true,
        --             forkId = Nothing,
        --             stepsBackward = stepsBackward + 1
        --           }
      -- handleAction Forward = undefined
        -- {stepsBackward} <- H.get
        -- when (stepsBackward /= 0) $ do 
        --   logDebug $  loc <> " ---> forward"
        --   { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        --   for_ (user :: Maybe User) \{ token } -> do
        --     {timeline, stepsBackward} <- H.get
        --     for_ (last timeline) \ps@{end: p@{hour, min}} -> do
        --       -- logDebug $ loc <> " --->  forward. points " <> show ps
        --       time <- H.liftEffect $ nowTime
        --       let minutes = fromEnum $ minute time

        --       logDebug $ loc <> " --->  forward. end point " <> show p
        --       logDebug $ loc <> " --->  forward. current point {" <> show (Time.hour time) <> ", " <> show (minute time) <> "}"
        --       logDebug $ loc <> " --->  forward. current shift backwards " <> show stepsBackward 

        --       let diffMin = 
        --             ((hour + stepsBackward) * 60 + min) -
        --             (fromEnum (Time.hour time) * 60 + minutes)

        --       logDebug $ loc <> " --->  forward, diff " <> show diffMin

        --       let gap | 0 <= diffMin && diffMin < 5 = 0
        --               | 0 > diffMin && diffMin > -60 = abs diffMin + mod (60 - abs diffMin) 5
        --               | otherwise = floor $ toNumber (abs diffMin / 60)

        --       logDebug $ loc <> " --->  forward. gap " <> show gap

        --       let doNoGap from to point = do
        --             logDebug $ loc <> " --->  forward, doNoGap from -> to " <> show from <> ", " <> show to
        --             logDebug $ loc <> " --->  forward. doNoGap, point " <> show point
        --             { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        --             for_ (user :: Maybe User) \{ token } -> do
        --               resp <- Request.makeAuth (Just token) host Back.mkFrontApi $
        --                 Back.fetchTimelineForParticularHour Back.Forward point
        --               let failure = Async.send <<< flip Async.mkException loc  
        --               onFailure resp failure \{success: gaps} -> do
                        
        --                 let checkForward = if stepsBackward - 1 == 0 then false else true

        --                 forkId <- 
        --                   if not checkForward
        --                   then map Just $ H.fork $ do
        --                         logDebug $ loc <> " ---> timeline updater has been activated"
        --                         forever $ delay *> handleAction Update
        --                  else pure Nothing

        --                 let newTimeline = 
        --                       flip populatGaps gaps $ 
        --                         initTimeline from to
        --                 logDebug $ loc <> " --->  forward. current timline " <> 
        --                   show (map (\x -> x # Back._elements %~ map Back.printGapItemUnit # Back._amounts %~ map Back.printGapItemAmount) newTimeline) 
        --                 H.modify_ _ 
        --                   { timeline = newTimeline,
        --                     isForward = checkForward,
        --                     forkId = forkId,
        --                     stepsBackward = stepsBackward - 1,
        --                     isBackward = true
        --                   }

        --       let addGapTomin s g = if s + g > 60 then 0 else s + g
        --       let addGapToHour adjMin h = if adjMin == 0 then h + 1 else h 
        --       let -- if the gap is less then an hour we just need to adjust end point by thereof
        --           -- example: supposing the initial end point were 9.45, current time were 10.55
        --           -- the gap is 10 min
        --           -- new start point: 9.55 (9.45 + 10 min), end point 10.55
        --           gapLessThenHour 
        --             | stepsBackward > 1 = 
        --                 let point = show hour <> "," <> show min
        --                 in doNoGap ((setTime min hour) time) (setTime min (hour + 1) time) point
        --             | otherwise = do            
        --                 logDebug $ loc <> " --->  forward, gapLessThenHour"
        --                 let newMin = addGapTomin min gap
        --                 let newHour = addGapToHour newMin hour
        --                 let point = show newHour <> "," <> show newMin
        --                 let minutesAdj = minutes + mod (60 - minutes) 5
        --                 doNoGap (setTime newMin newHour time) (setMinute (intToTimePiece minutesAdj) time) point
        --       let -- in this case just add an hour and then two hours to the end point
        --           -- example: supposing the initial end point were 9.45, current time were 11.50
        --           -- we have a gap of 2 hour and 5 min, 
        --           -- new start point: 10.45 (9.45 + 1 hour), end point 10.45 (9.45 + 2 hours)
        --           gapMoreThen2Hour =
        --             let msg = "gapMoreThen2Hour hasn't been implemented yet. reload page"
        --             in Async.send $ Async.mkOrdinary msg Async.Debug (Just loc) 

        --       let doWithGap | 0 > diffMin && diffMin > -60 = gapLessThenHour
        --                     | otherwise = gapMoreThen2Hour

        --       if 0 <= diffMin && diffMin < 5
        --       then
        --         let point = show hour <> "," <> show min
        --         in doNoGap (setTime min hour time) (setTime min (hour + 1) time) point
        --       else doWithGap       
     
      handleAction (UpdateTransaction {hour, min, textualIdent: ident, status: newStatus}) = undefined
        -- {timeline} <- H.get
        -- let newTimeline = 
        --       flip map timeline \x ->
        --         if x^.Back._start <<< Back._hour * 60 + 
        --            x^.Back._start <<< Back._min <= 
        --            hour * 60 + min
        --            && x^.Back._end <<< Back._hour * 
        --               60 + x^.Back._end <<< Back._min >= 
        --            hour * 60 + min
        --         then x # Back._elements %~ \xs -> 
        --                flip map xs \el@{textualIdent} -> 
        --                  if ident ==  textualIdent 
        --                  then el { status = newStatus } 
        --                  else el
        --         else x
        -- H.modify_ _ { timeline = newTimeline }

      handleAction (UpdateWallet wallet@{ident, amount: new}) = do
        logDebug $ loc <> " wallet update --- >" <> show wallet
        {wallets: old} <- H.get
        H.modify_ _ { wallets = flip map old \x -> if _.ident x == ident then x { amount = new } else x }
      handleAction Finalize = do
        map (_.forkId) H.get >>= flip for_ H.kill
        { wsVar } <- getStore
        wsm <- H.liftEffect $ Async.tryTake wsVar
        for_ wsm \xs ->
          for_ xs \{ ws, forkId } -> do
            H.kill forkId
            H.liftEffect $ WS.close ws
            logDebug $ loc <> " ---> ws has been killed"
      handleAction (HandleChild (Timeline.OutputDirection Back.Backward timeline)) = handleBackward timeline
      handleAction (HandleChild (Timeline.OutputDirection Back.Forward timeline)) = undefined


handleBackward timeline = do 
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do
    for_ (uncons timeline) \{head: {start: p@{hour, min}}} -> do
      let point = show hour <> "," <> show min
      resp <- Request.makeAuth (Just token) host Back.mkFrontApi $
        Back.fetchTimelineForParticularHour Back.Backward point
      let failure = Async.send <<< flip Async.mkException loc  
      onFailure resp failure \{success: gaps} -> ok gaps p
  where 
    ok gaps {hour, min} = 
        do
          {stepsBackward} <- H.get
          map (_.forkId) H.get >>= flip for_ H.kill
          logDebug $ loc <> " --->  backward, gaps " <> show (Back.printTimline gaps)
          time <- H.liftEffect $ nowTime
          let newStartPoint 
                | hour - 1 < 0 = Timeline.setTime min 23 time 
                | otherwise = Timeline.setTime min (hour - 1) time
          let newEndPoint = Timeline.setTime min hour time
          logDebug $ loc <> " --->  backward. new points " <> show newStartPoint <> ", " <> show newEndPoint
          let newTimeline = 
                flip Timeline.populatGaps gaps $ 
                  Timeline.initTimeline newStartPoint newEndPoint
          logDebug $ loc <> " --->  backward. current timline " <> show (Back.printTimline newTimeline)
          H.modify_ _ { forkId = Nothing, stepsBackward = stepsBackward + 1 }
          H.tell Timeline.proxy 1 $ Timeline.NewTimeline newTimeline (if hour - 1 < 0 then false else true) true

resolveEnums :: Back.Wallet -> Back.EnumResolvedWallet
resolveEnums x = 
  x { walletType = 
       fromMaybe Back.WalletTypeNotResolved $
         Back.decodeWalletType (_.walletType x),
     currency =
        fromMaybe Back.CurrencyNotResolved $
         Back.decodeCurrency (_.currency x) 
    }

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
render { timeline: [] } = HH.div_ [HH.text "loading..."]
render { error: Nothing, wallets, timeline, institution } =
  HH.div_ 
  [
      HH.div [css "wallet-container"] $ renderWallets wallets
  ,   Timeline.slot 1 {timeline: timeline, institution: institution} HandleChild
  ]