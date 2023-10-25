module BCorrespondent.Component.Workspace.BalancedBook (slot, Output (..)) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, maybeElem)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Component.Workspace.BalancedBook.Timeline as Timeline
import BCorrespondent.Component.Workspace.BalancedBook.Amount as Amount

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Halogen.HTML.Core (HTML)
import Type.Proxy (Proxy(..))
import Data.Array (zip, (..), (:), snoc, head, foldl, length)
import Data.Lens (_1, _2, (^.))
import Data.Generic.Rep (class Generic)
import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Enum.Generic 
       (genericCardinality, genericToEnum, 
        genericFromEnum, genericSucc, genericPred)
import Data.Maybe (fromMaybe, Maybe (..), maybe)
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Store (User)
import Effect.Exception (message)
import Data.Lens ((^.), _2)
import System.Time (addDays, nowDate, nowTime)
import Data.DateTime as D
import Effect.Aff as Aff
import Control.Monad.Rec.Class (forever)
import Data.String (split)
import Data.String.Pattern (Pattern (..))
import Data.Int (fromString)
import Data.Array (index)

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


data Action = 
       Initialize 
     | LoadTimeline Int Int Int 
     | HandleChildTimeline Timeline.Output
     | ShowAmount (Array Back.ForeignDayOfWeeksHourlyTotalSum)
     | LoadWeek Back.Direction

type Timeline = { date :: String, from :: Int }

type Now = { weekday :: Int, hour :: Int } 

type State = 
     { book :: Maybe Back.BalancedBook, 
       error :: Maybe String,
       timeline :: Maybe Timeline,
       now :: Now
     }

component =
  H.mkComponent
    { initialState: 
      const 
      { book: Nothing, 
        error: Nothing, 
        timeline: Nothing,
        now: { weekday: 0, hour: 0 }
      }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      }
    }

handleAction Initialize = do
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do 
    resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
      Back.initBalancedBook
    let faiure e = H.modify_ _ { error = pure e } 
    onFailure resp faiure \{success: book} -> do
      dow <- map (fromEnum <<< D.weekday) $ H.liftEffect nowDate
      currHour <- map (fromEnum <<< D.hour) $ H.liftEffect nowTime
      H.modify_ _ { book = pure book, now = {weekday: dow, hour: currHour}  }

      void $ H.fork $ forever $ do
        H.liftAff $ Aff.delay $ Aff.Milliseconds 60000.0
        dow <- map (fromEnum <<< D.weekday) $ H.liftEffect nowDate
        currHour <- map (fromEnum <<< D.hour) $ H.liftEffect nowTime
        {now: {weekday, hour}} <- H.get
        when (hour /= currHour || 
              dow /= weekday) $
          H.modify_ _ { now = {weekday: dow, hour: currHour} }
handleAction (LoadTimeline amount idx hour) = do 
  {book, now: {weekday, hour: currHour}} <- H.get
  if amount == 0 && idx /= weekday
  then pure unit
  else if idx == weekday
  then H.raise $ OutputLive currHour
  else if amount /= 0 
  then
    for_ book \{from} -> do
      let days 
           | toEnum idx == Just Monday || 
             toEnum idx == Just Sunday = 0
           | otherwise = idx - 1
      from' <- H.liftEffect $ addDays days from
      let msg = 
            loc <> " ----> loading timeline for " <> 
            from' <> ", time gap: " <>
            show hour <> "-" <> show (hour + 1)
      logDebug msg
      H.modify_ _ { timeline = Just { date: from', from: hour } }
  else pure unit    
handleAction (HandleChildTimeline Timeline.OutputBack) = H.modify_ _ { timeline = Nothing }
handleAction (ShowAmount xs) = H.tell Amount.proxy 0 $ Amount.Open xs
handleAction (LoadWeek direction) = do
  {book} <- H.get
  for_ book \{from, to} -> do
    { config: Config { apiBCorrespondentHost: host }, user } <- getStore
    for_ (user :: Maybe User) \{ token } -> do
      let point | direction == Back.Forward = to
                | otherwise = from
      let dateXs = split (Pattern "-") point <#> fromString
      let dateRecord = do
           y <- join $ index dateXs 2
           m <- join $ index dateXs 1
           d <- join $ index dateXs 0
           pure { year: y, month: m, day: d }
      for_ dateRecord \{ year, month, day } -> do 
        resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
          Back.fetchBalancedBook year month day direction
        let faiure e = H.modify_ _ { error = pure e }   
        onFailure resp faiure \{success: book} -> H.modify_ _ { book = pure book }

render { book: Nothing, error: Nothing } = 
  HH.div [css "book-container"] [HH.text "book loading..."]
render { book: _, error: Just e } = 
  HH.div [css "book-container"] [ HH.text $ "error occured during loading: " <> message e ]
render { book: Just { from, to, institutions: xs }, timeline: Nothing, now } = 
  HH.div [css "book-container"] 
  [
      HH.div_ [HH.h2_ [HH.text $ "accounting period: " <> from <> " - " <> to]]
  ,   maybeElem (head xs) $ renderTimeline now
  ]
render { book: Just _, timeline: Just {date, from} } = 
  HH.div [css "book-container"] [Timeline.slot 1 {date: date, from: from} HandleChildTimeline]


type Row = { shift :: Int, rows :: forall w i . Array (HTML w i) }

renderTimeline now {title, dayOfWeeksHourly: xs} =
  HH.div_ $
  ([
      Amount.slot 0
  ,   HH.div [css "book-timeline-title"] [HH.text title]
  ,   HH.div [css "book-timeline"] $
        HH.span 
        [css "book-timeline-item", 
         HPExt.style "border-left: 1px solid black"]
        [HH.text "time"] : 
        ((fromEnum Monday .. fromEnum Sunday) <#> \idx ->
          HH.span [css "book-timeline-item" ]
          [HH.text $ show $ 
            fromMaybe undefined ((toEnum idx) :: Maybe DayOfWeek)
        ]) `snoc` HH.span [css "book-timeline-item" ] [HH.text "total"]
  ] <> 
  (_.rows $ foldl (renderRow now) { shift: 100, rows: [] } (xs :: Array Back.DayOfWeekHourly) )) <>
  [
      HH.span 
      [HE.onClick (const (LoadWeek Back.Backward)), 
       css "balanced-book-travel-button-previous"] 
      [HH.text "previous week"]
  ,   HH.span 
      [HE.onClick (const (LoadWeek Back.Forward)), 
       css "balanced-book-travel-button-next"] 
      [HH.text "next week"]
  ]

renderRow {weekday, hour} {shift: oldShift, rows} {to, from, amountInDayOfWeek: xs, total: ys} =
  let newShift = oldShift + 20
      renderTotal {amount, currency} = show amount <> "..."
      x = 
          HH.div [css "book-timeline", HPExt.style $ "top:" <> show newShift <> "px" ] $
            HH.span 
            [css "book-timeline-item", 
             HPExt.style "border-left: 1px solid black"]
             [HH.text $ show (_.hour from) <> " - " <> show (_.hour to)] :
            (xs <#> \{total, value: dow} -> 
              let text = 
                    if total == 0
                    then "-"
                    else show total
                  isNow = 
                    dow == weekday && 
                    _.hour from == hour  
                  style | total > 0 && dow == weekday = "book-timeline-item-today"
                        | total > 0 && not isNow = "book-timeline-item-past"
                        | isNow = "book-timeline-item-active-live pulse-live"
                        | otherwise = "book-timeline-item"
              in HH.span [css style, HE.onClick (const (LoadTimeline total dow (_.hour from)))] [HH.text text])
             `snoc`
             let style | length ys > 0 = "book-timeline-item-past pulse"
                       | otherwise = "book-timeline-item" 
             in HH.span
                [css style,
                 HPExt.style "border-right: 1px solid black",
                 HE.onClick (const (ShowAmount ys)) ] 
                [HH.text (maybe "-" renderTotal (head ys))]
              
  in { shift: newShift, rows: rows `snoc` x }