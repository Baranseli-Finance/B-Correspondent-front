module Buzgibi.Component.Lang.Data where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Enum
import Data.Bounded
import Data.Enum.Generic (genericFromEnum, genericToEnum, genericSucc, genericPred, genericCardinality)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)

data Lang = Eng | Turk

derive instance genericLang :: Generic Lang _
derive instance eqLang :: Eq Lang
derive instance ordLang :: Ord Lang

instance encodeJsonLang :: EncodeJson Lang where
  encodeJson Eng = encodeJson "english"
  encodeJson Turk = encodeJson "turkish"

instance showLang :: Show Lang where
  show Eng = "English"
  show Turk = "Türkçe"

instance enumLang :: Enum Lang where
  succ = genericSucc
  pred = genericPred

instance boundedEnumLang :: BoundedEnum Lang where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance boundedLang :: Bounded Lang where
  top = Turk
  bottom = Eng

data Action = Notify Int | Initialize

type State = { lang :: Int }

data Recipients = Home | Menu

instance showRecipients :: Show Recipients where
  show Home = "home"
  show Menu = "menu"

instance encodeJsonRecipients :: EncodeJson Recipients where
  encodeJson Home = encodeJson "home"
  encodeJson Menu = encodeJson "menu"

derive instance genericRecipients :: Generic Recipients _
derive instance eqRecipients :: Eq Recipients
derive instance ordRecipients :: Ord Recipients

instance enumRecipients :: Enum Recipients where
  succ = genericSucc
  pred = genericPred

instance boundedEnumRecipients :: BoundedEnum Recipients where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance boundedRecipients :: Bounded Recipients where
  top = Menu
  bottom = Home