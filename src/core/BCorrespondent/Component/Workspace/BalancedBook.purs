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

import Undefined

proxy = Proxy :: _ "workspace_balanced_book"

loc = "BCorrespondent.Component.Workspace.BalancedBook"

slot n = HH.slot_ proxy n component unit

data Week = 
        Monday 
      | Tuesday 
      | Wednesday 
      | Thursday 
      | Friday 
      | Saturday 
      | Sunday

derive instance genericWeek :: Generic Week _
derive instance eqWeek :: Eq Week
derive instance ordWeek :: Ord Week

instance Enum Week where
  succ = genericSucc
  pred = genericPred

instance Bounded Week where
  top = Monday
  bottom = Sunday

instance BoundedEnum Week where
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
    HH.div [css "book-timeline-item"] [HH.text "time"] : 
    (zip (0 .. 23) (1 .. 24) <#> \tpl -> 
      HH.div [css "book-timeline-item"] 
      [HH.text $ 
         show (tpl^._1) <> "-" <> 
         show (if tpl^._2 == 24 then 0 else tpl^._2)
      ])