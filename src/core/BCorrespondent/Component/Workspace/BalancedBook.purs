module BCorrespondent.Component.Workspace.BalancedBook (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Data.Array (zip, (..), (:))
import Data.Lens (_1, _2, (^.))
import Data.Generic.Rep (class Generic)
import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Enum.Generic 
       (genericCardinality, genericToEnum, 
        genericFromEnum, genericSucc, genericPred)
import Data.Maybe (fromMaybe, Maybe)

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

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }


render = HH.div [css "book-container"] [ renderTimeline ]

renderTimeline = 
  HH.div [css "book-timeline"] $ 
    HH.span 
    [css "book-timeline-item", 
     HPExt.style "border-left: 1px solid black"]
    [HH.text "time"] : 
      ((fromEnum Monday .. fromEnum Sunday) <#> \idx ->
        HH.span [css "book-timeline-item" ]
        [HH.text $ show $ 
          fromMaybe undefined ((toEnum idx) :: Maybe DayOfWeek)
      ])