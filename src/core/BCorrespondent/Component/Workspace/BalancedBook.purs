module BCorrespondent.Component.Workspace.BalancedBook (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, maybeElem)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Data.Config (Config(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Core (HTML)
import Type.Proxy (Proxy(..))
import Data.Array (zip, (..), (:), snoc, head, foldl)
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

import Undefined

proxy = Proxy :: _ "workspace_balanced_book"

loc = "BCorrespondent.Component.Workspace.BalancedBook"

slot n = HH.slot_ proxy n component unit

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

data Action = Initialize

type State = { book :: Maybe Back.BalancedBook, error :: Maybe String }

component =
  H.mkComponent
    { initialState: 
      const { book: Nothing, error: Nothing }
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
    onFailure resp faiure \{success: book} -> H.modify_ _ { book = pure book }  


render { book: Nothing, error: Nothing } = 
  HH.div [css "book-container"] [HH.text "book loading..."]
render { book: Nothing, error: Just e } = 
  HH.div [css "book-container"] [ HH.text $ "error occured during loading: " <> message e ]
render { book: Just { from, to, institutions: xs } } = 
  HH.div [css "book-container"] 
  [
      HH.div_ [HH.h2_ [HH.text $ "acconting period: " <> from <> " - " <> to]]
  ,   maybeElem (head xs) renderTimeline
  ]

type Row = { shift :: Int, rows :: forall w i . Array (HTML w i) }

renderTimeline {title, dayOfWeeksHourly: xs} =
  HH.div_
  ([
      HH.div [css "book-timeline-title"] [HH.text title]
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
  ] <> (_.rows $ foldl renderRow { shift: 100, rows: [] } (xs :: Array Back.DayOfWeekHourly) ))

renderRow {shift: oldShift, rows} {to, from, amountInDayOfWeek: xs, total: ys} =
  let newShift = oldShift + 20
      renderTotal {amount, currency} = show amount <> "..."
      x = 
          HH.div [css "book-timeline", HPExt.style $ "top:" <> show newShift <> "px" ] $
            HH.span 
            [css "book-timeline-item", 
             HPExt.style "border-left: 1px solid black"]
             [HH.text $ show (_.hour from) <> " - " <> show (_.hour to)] :
            (xs <#> \{total} -> HH.span [css "book-timeline-item" ] [HH.text (show total)])
             `snoc` 
            HH.span
            [css "book-timeline-item",
             HPExt.style "border-right: 1px solid black"] 
            [HH.text (maybe "0" renderTotal (head ys))]
              
  in { shift: newShift, rows: rows `snoc` x }