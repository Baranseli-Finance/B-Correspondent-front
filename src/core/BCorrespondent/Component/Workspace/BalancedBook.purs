module BCorrespondent.Component.Workspace.BalancedBook (slot, Output (..)) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, maybeElem, whenElem)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Component.Workspace.BalancedBook.Timeline as Timeline
import BCorrespondent.Component.Workspace.BalancedBook.Amount as Amount
import BCorrespondent.Component.Subscription.WS as WS
import BCorrespondent.Component.Subscription.WS.Types (TransactionBalancedBook, WalletBalancedBook)
import BCorrespondent.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Halogen.HTML.Core (HTML)
import Type.Proxy (Proxy(..))
import Data.Array 
       (zip, (..), (:), snoc, head, foldl, length, 
        index, singleton, findIndex, modifyAt, reverse, find,
        foldM
       )
import Data.Lens (_1, _2, (^.))
import Data.Generic.Rep (class Generic)
import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Enum.Generic 
       (genericCardinality, genericToEnum, 
        genericFromEnum, genericSucc, genericPred)
import Data.Maybe (fromMaybe, Maybe (..), maybe)
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Store (User, WS)
import Effect.Exception (message)
import Data.Lens ((^.), _2, (%~))
import Data.Lens.Traversal (traversed)
import System.Time (addDays, nowDate, nowTime)
import Data.DateTime as D
import Effect.Aff as Aff
import Control.Monad.Rec.Class (forever)
import Data.String (split, take)
import Data.String.Pattern (Pattern (..))
import Data.Int (fromString, floor)
import Data.DateTime as D
import Web.Socket as WS
import Effect.AVar as Async
import AppM (AppM)
import Data.Maybe (fromMaybe)
import Data.Tuple (uncurry)
import Crypto.Jwt (fetchInstitution)
import Data.Decimal (fromNumber, toFixed)


import Undefined

proxy = Proxy :: _ "workspace_balanced_book"

loc = "BCorrespondent.Component.Workspace.BalancedBook"

slot n = HH.slot proxy n component unit

data Output = OutputLive Int

data DayOfWeek = 
        Monday 
      | Tuesday 
      | Wednesday 
      | Thursday 
      | Friday 
      | Saturday 
      | Sunday

derive instance genericDayOfWeek :: Generic DayOfWeek _
derive instance eqDayOfWeek :: Eq DayOfWeek
derive instance ordDayOfWeek :: Ord DayOfWeek

instance Show DayOfWeek where
  show Monday = "monday"
  show Tuesday = "tuesday"
  show Wednesday = "wednesday"
  show Thursday = "thursday"
  show Friday = "friday"
  show Saturday = "saturday"
  show Sunday = "sunday"


instance Enum DayOfWeek where
  succ = genericSucc
  pred = genericPred

instance Bounded DayOfWeek where
  top = Monday
  bottom = Sunday

instance BoundedEnum DayOfWeek where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

type Slot q = 
      (workspace_balanced_book_amount :: H.Slot Amount.Query Unit Int, 
       workspace_balanced_book_timeline :: H.Slot q Timeline.Output Int 
      )

data Action = 
       Initialize
     | Finalize
     | LoadTimeline Int Int Int Int
     | HandleChildTimeline Timeline.Output
     | ShowAmount Int (Array Back.ForeignDayOfWeeksHourlyTotalSum)
     | LoadWeek Back.Direction
     | AddTransaction TransactionBalancedBook
     | UpdateWallets (Array WalletBalancedBook)

type Timeline = { date :: String, from :: Int, institution :: Int }

type Now = { weekday :: Int, hour :: Int } 

type State = 
     { book :: Maybe Back.BalancedBook, 
       error :: Maybe String,
       timeline :: Maybe Timeline,
       now :: Now,
       isPast :: Boolean,
       canTravelBack :: Boolean,
       isLoading :: Boolean
     }

type Row = { shift :: Int, rows :: forall w i . Array (HTML w i) }

type BookCSS =
     { institution :: String, 
       wallet :: String,
       container :: String,
       position :: Int
     }

component =
  H.mkComponent
    { initialState: const 
      { book: Nothing, 
        error: Nothing, 
        timeline: Nothing,
        now: { weekday: 0, hour: 0 },
        isPast: false,
        canTravelBack: true,
        isLoading: false
      }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      , finalize = pure Finalize
      }
    }

canMoveBack from to = do 
  {since: ({year, month, day} :: Back.InvoiceSince) } <- getStore
  let mkNow = show year <> "-" <> show month <> "-" <> show day
  pure $ not $ mkNow >= from && mkNow <= to


handleAction :: forall q . Action -> H.HalogenM State Action (Slot q) Output AppM Unit
handleAction Initialize = do
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do 
    resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
      Back.initBalancedBook
    let faiure e = H.modify_ _ { error = pure $ message e } 
    onFailure resp faiure \{success: book@{from, to}} -> do
      dow <- map (fromEnum <<< D.weekday) $ H.liftEffect nowDate
      currHour <- map (fromEnum <<< D.hour) $ H.liftEffect nowTime
      
      can <- canMoveBack from to

      H.modify_ _ { book = pure book, now = {weekday: dow, hour: currHour}, canTravelBack = can }

      void $ H.fork $ forever $ do
        H.liftAff $ Aff.delay $ Aff.Milliseconds 60000.0
        dow <- map (fromEnum <<< D.weekday) $ H.liftEffect nowDate
        currHour <- map (fromEnum <<< D.hour) $ H.liftEffect nowTime
        {now: {weekday, hour}} <- H.get
        when (hour /= currHour || 
              dow /= weekday) $
          H.modify_ _ { now = {weekday: dow, hour: currHour} }

      WS.subscribe loc WS.transactionBalancedBookUrl (Just (WS.encodeResource WS.BalancedBookTransaction)) $
        handleAction <<< AddTransaction <<< _.success

      WS.subscribe loc WS.walletBalancedBookUrl (Just (WS.encodeResource WS.BalancedBookWallet)) $
        handleAction <<< UpdateWallets <<< _.success
handleAction Finalize = do
  { wsVar } <- getStore
  wsm <- H.liftEffect $ Async.tryTake (wsVar :: Async.AVar (Array WS))
  for_ wsm \xs -> do
    let release ys y@{ ws, forkId, component: l}
          | l == loc = do
              H.kill forkId
              H.liftEffect $ WS.close ws
              pure ys
          | otherwise = pure $ y : ys
    logDebug $ loc <> " ---> ws has been killed"
    xs' <- foldM release [] xs
    H.liftEffect $ xs' `Async.tryPut` wsVar 
  logDebug $ loc <> " component is closed"       
handleAction (LoadTimeline institution amount idx hour) = do
  logDebug $ loc <> " ----> loading timeline, raw data: weekday idx - " <> show idx <> ", hour - " <> show hour
  {book, now: {weekday, hour: currHour}, isPast} <- H.get
  {user} <- getStore
  for_ user \{jwtUser: {institution: identf} } -> do 
    res <- fetchInstitution identf
    for_ res \ident ->
      if amount == 0 && (idx /= weekday || isPast)
      then pure unit
      else if idx == weekday && ident == institution && not isPast
      then H.raise $ OutputLive currHour
      else if amount /= 0  && ident == institution
      then
        for_ book \{from} -> do
          let days = idx - 1
          logDebug $ loc <> " ----> loading timeline, days: " <> show days <> ", from " <> show from      
          from' <- H.liftEffect $ addDays days from
          let msg = 
                  loc <> " ----> loading timeline for " <> 
                  from' <> ", time gap: " <>
                  show hour <> "-" <> show (hour + 1)
          logDebug msg
          H.modify_ _ { timeline = Just { date: from', from: hour, institution: institution } }
      else let msg = "you should be granted an additional right to monitor the second part"
           in Async.send $ Async.mkOrdinary msg Async.Error Nothing
handleAction (HandleChildTimeline Timeline.OutputBack) = H.modify_ _ { timeline = Nothing }
handleAction (ShowAmount idx xs) = H.tell Amount.proxy idx $ Amount.Open xs
handleAction (LoadWeek direction)
  | direction == Back.Forward = do 
      {isPast} <- H.get
      when isPast $ do
        H.modify_ _ { isLoading = true } 
        fetchBalancedBook direction
        H.modify_ _ { isLoading = false }
  | otherwise = do 
      {canTravelBack} <- H.get
      when canTravelBack $ do
        H.modify_ _ { isLoading = true }
        fetchBalancedBook direction
        H.modify_ _ { isLoading = false }
handleAction (AddTransaction t@transactiion) = do
  {isPast} <- H.get
  when (not isPast) $ do
    logDebug $ loc <> " transaction update ---> " <> 
      show (transactiion { currency = Back.decodeCurrency (_.currency t) })
    H.modify_ \s -> 
      s { book = _.book s <#> \b -> 
          b # Back._institutionBalancedBook %~ modify 
        }
  where 
    modify xs = 
      xs <#> \x@{title} -> 
        if title /= _.institutionTitle t 
        then x
        else x { dayOfWeeksHourly = 
                   _.dayOfWeeksHourly x <#> \y@{from, to} -> 
                     if _.hour from == _.from t 
                        && _.hour to == _.to t
                     then y {amountInDayOfWeek =
                              let idxm = flip findIndex (_.amountInDayOfWeek y) \{value} -> value == _.dow t
                                  add i = flip (modifyAt i) (_.amountInDayOfWeek y) \z -> z { total = _.total z + 1 }
                              in maybe (singleton { value: _.dow t, total: 1 }) (fromMaybe undefined <<< add) idxm,
                             total =
                              let idxm = 
                                    flip findIndex (_.total y) \{currency} -> 
                                      Back.decodeCurrency currency ==
                                      Back.decodeCurrency (_.currency t)
                                  add i = flip (modifyAt i) (_.total y) \z -> z { amount = _.amount z + _.amount t }
                              in maybe (singleton { currency: _.currency t, amount: _.amount t }) (fromMaybe undefined <<< add) idxm
                            }
                     else y
               }
handleAction (UpdateWallets wallets) = do 
  logDebug $ loc <> " wallets update ---> " <> show wallets
  H.modify_ \s -> 
    s { book = _.book s <#> \b -> 
        b # Back._institutionBalancedBook
            <<< traversed 
            <<< Back._balancesBalancedBook
            <<< traversed
        %~ modify 
      }
  where
    modify w@{ident} =
      let newAmount = 
             fromMaybe (_.amount w) $ 
               map _.amount $ 
                 flip find wallets \w' -> 
                   _.ident w' == ident
      in w { amount = newAmount }
    

fetchBalancedBook direction = do
  {book} <- H.get
  for_ book \{from, to} -> do
    { config: Config { apiBCorrespondentHost: host }, user } <- getStore
    for_ (user :: Maybe User) \{ token } -> do
      let point | direction == Back.Forward = to
                | otherwise = from
      let dateXs = split (Pattern "-") point <#> fromString
      let dateRecord = do
            y <- join $ index dateXs 0
            m <- join $ index dateXs 1
            d <- join $ index dateXs 2
            pure { year: y, month: m, day: d }
      for_ dateRecord \{ year, month, day } -> do 
        resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
          Back.fetchBalancedBook year month day direction
        let faiure e = H.modify_ _ { error = pure $ message e }
        onFailure resp faiure \{success: book@{from, to}} -> do
          can <- canMoveBack from to
          now <- H.liftEffect nowDate
          let month = 
                if fromEnum (D.month now) < 10 
                then "0" <> show (fromEnum (D.month now)) 
                else show (fromEnum (D.month now))
          let day =
                if fromEnum (D.day now) < 10 
                then "0" <> show (fromEnum (D.day now)) 
                else show (fromEnum (D.day now))    
          let mkNowDate = 
                show (fromEnum (D.year now)) <> "-" <> 
                month <> "-" <> day  
          let isPast | mkNowDate >= from && mkNowDate <= to = false
                     | otherwise = true
          logDebug $ loc <> " fetchBalancedBook ---> now: " <> mkNowDate <> ", from: " <> from <> ", to: " <> to
          H.modify_ _ { book = pure book, isPast = isPast, canTravelBack = can }


bookCSSxs = 
  [ { institution: "book-timeline-title",
      container: "book-timeline",
      wallet: "balanced-book-wallets",
      position: 1 }
  , { institution: "book-timeline-title-2", 
      container: "book-timeline-2",
      wallet: "balanced-book-wallets-2",
      position: 2 }
  ]

render { book: Nothing, error: Nothing } = 
  HH.div [css "book-container"] [HH.div [css "loader"] []]
render { book: _, error: Just e } = 
  HH.div [css "book-container"] [ HH.text $ "error occured during loading: " <> e ]
render { book: Just { from, to, institutions: xs }, timeline: Nothing, now, isPast, canTravelBack, isLoading } =
  HH.div [css "book-container"] $
  [
      whenElem isLoading $ HH.div [css "loader"] []  
  ,   HH.div_ [HH.h2_ [HH.text $ "accounting period: " <> from <> " - " <> to]]  
  ] <>
  (zip bookCSSxs xs <#> uncurry (renderTimeline now isPast canTravelBack)) <>
  [  
      HH.span 
      [HE.onClick (const (LoadWeek Back.Backward)), 
       css "balanced-book-travel-button-previous", 
       HPExt.style $ "cursor:" <> if canTravelBack then "pointer" else "not-allowed" ]
      [HH.h2_ [HH.text "prev week"]]
  ,   HH.span 
      [HE.onClick (const (LoadWeek Back.Forward)), 
       css "balanced-book-travel-button-next", 
       HPExt.style $ "cursor:" <> if isPast then "pointer" else "not-allowed" ] 
      [HH.h2_ [HH.text "next week"]]
  ]
render { book: Just _, timeline: Just {date, from, institution} } = 
  HH.div [css "book-container"] [Timeline.slot 0 {date: date, from: from, institution: institution} HandleChildTimeline]

renderTimeline now isPast canTravelBack {institution, position, wallet, container} {title, ident, dayOfWeeksHourly: xs, balances: ys} =
  HH.div [HPExt.style hiddenStyle] $
  ([
      Amount.slot position
  ,   HH.div [css institution] [HH.text title]
  ,   HH.div [css container] $
        HH.span 
        [css "book-timeline-item", 
         HPExt.style "border-left: 1px solid black"]
        [HH.text "time"] `timeOp`
        (weekdays <#> \idx ->
          HH.span [css "book-timeline-item" ]
          [HH.text $ show $ 
            fromMaybe undefined ((toEnum idx) :: Maybe DayOfWeek)
        ]) `totalOp` HH.span [css "book-timeline-item" ] [HH.text "total"]
  ] <> 
  (_.rows $ foldl (renderRow position ident container now isPast) { shift: 100, rows: [] } (xs :: Array Back.DayOfWeekHourly) )) <>
  [HH.div [css wallet] (_.rows $ foldl (renderWallet position) { shift: 100, rows: [] } (ys :: Array Back.Balances) )]
  where weekdays | position == 1 = (fromEnum Monday .. fromEnum Sunday)
                 | otherwise = (fromEnum Sunday .. fromEnum Monday)
        timeOp | position == 1 = (:)
               | otherwise = flip snoc
        totalOp | position == 1 = snoc
                | otherwise = flip (:)
        hiddenStyle = if position == 2 then "background-color:white;opacity:0.5" else mempty        

renderRow position ident container {weekday, hour} isPast {shift: oldShift, rows} {to, from, amountInDayOfWeek: xs, total: ys} =
  let newShift = oldShift + 20
      totalOp | position == 1 = snoc
              | otherwise = flip (:)
      timeOp |  position == 1 = (:)
             | otherwise = flip snoc
      timeStyle | position == 1 = "border-left: 1px solid black"
                | otherwise = "border-right: 1px solid black"  
      renderTotal {amount, currency} 
        | floor amount > 999_999 = take 6 (toFixed 2 (fromNumber amount)) <> "..."
        | otherwise = toFixed 2 (fromNumber amount)
      x = 
          HH.div [css container, HPExt.style $ "top:" <> show newShift <> "px" ] $
            HH.span 
            [css "book-timeline-item", 
             HPExt.style timeStyle]
             [HH.text $ show (_.hour from) <> " - " <> show (_.hour to)] `timeOp`
            (xs <#> \{total, value: dow} -> 
              let text = 
                    if total == 0
                    then "-"
                    else show total
                  isNow = 
                    dow == weekday && 
                    _.hour from == hour
                  style | total > 0 && dow == weekday && not isPast = 
                           "book-timeline-item-today pointer-to-live-tm"
                        | total > 0 && (isPast || not isNow) = "book-timeline-item-past"
                        | otherwise = 
                            "book-timeline-item " <> 
                            if dow == weekday && not isPast 
                            then "pointer-to-live-tm" 
                            else mempty
                  pulseStyle = "book-timeline-item-active-live pulse-live"     
              in HH.span 
                 [css (if isNow && not isPast then pulseStyle else style), 
                  HE.onClick (const (LoadTimeline ident total dow (_.hour from)))] 
                  [HH.text text])
             `totalOp`
             let style | length ys > 0 = "book-timeline-item-past"
                       | otherwise = "book-timeline-item" 
             in HH.span
                [css style,
                 HPExt.style "border-right: 1px solid black",
                 HE.onClick (const (ShowAmount position ys)) ] 
                [HH.text (maybe "-" renderTotal (head ys))]
              
  in { shift: newShift, rows: rows `snoc` x }

renderWallet position record {amount, currency, walletType} = 
  let item =
         if position == 1
         then 
            [
                let text = 
                      show (Back.decodeWalletType walletType) <> "(" <> 
                      show (Back.decodeCurrency currency) <> ")"
                in HH.span [css "balanced-book-wallet-text"] [HH.text text]
            ,   HH.span [css "balanced-book-wallet-amount"] [HH.text (show amount)]
            ]
         else
            [
                HH.span [css "balanced-book-wallet-amount-2"] [HH.text (show amount)]
            ,   let text = 
                      show (Back.decodeWalletType walletType) <> "(" <> 
                      show (Back.decodeCurrency currency) <> ")"
                in HH.span [css "balanced-book-wallet-text-2"] [HH.text text]
            ] 
  in { shift: 0, rows: _.rows record `snoc` HH.div_ item }