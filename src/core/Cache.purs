module Cache
  ( Cache
  , init
  , writeTranslation
  , readTranslation
  ) where

import Prelude

import Buzgibi.Api.Foreign.BuzgibiBack (Translation)

import Data.Map as Map
import Data.Maybe (Maybe(..))

type TranslationValue = { value :: Translation, hash :: String }

type CacheImpl = { translation :: Maybe TranslationValue }

newtype Cache = Cache CacheImpl

instance Show Cache where
  show (Cache { translation: Nothing }) = "{cache: empty}"
  show (Cache { translation: Just { value, hash } }) = "{ cache: translation: { value: " <> show value <> ", hash: " <> hash <> " } }"

init :: Cache
init = Cache { translation: Nothing }

writeTranslation :: Translation -> String -> Cache -> Cache
writeTranslation x hash (Cache impl) = Cache $ impl { translation = Just { value: x, hash: hash } }

readTranslation :: Cache -> Maybe TranslationValue
readTranslation (Cache { translation: val }) = val
