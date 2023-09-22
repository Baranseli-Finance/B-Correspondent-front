module Buzgibi.Data.Config
  ( Config(..)
  , ConfigVal
  , getVal
  , setIsCaptcha
  , setShaCommit
  , setToTelegram
  ) where

import Prelude

import Store.Types (LogLevel)
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Decode.Error
import Data.Either
import Prim.Coerce (class Coercible)

type ConfigVal =
  { telegramBot :: String
  , telegramChat :: String
  , telegramHost :: String
  , toTelegram :: Boolean
  , apiBuzgibiHost :: String
  , apiBuzgibiHostWS :: String
  , sha256Commit :: String
  , cssLink :: String
  , traceLocation :: String
  , cssFiles :: Array String
  , isCaptcha :: Boolean
  }

newtype Config = Config ConfigVal

getVal (Config v) = v

instance EncodeJson Config where
  encodeJson
    ( Config
        { telegramBot
        , telegramChat
        , telegramHost
        , toTelegram
        , apiBuzgibiHost
        , sha256Commit
        , cssLink
        , traceLocation
        , cssFiles
        , isCaptcha
        , apiBuzgibiHostWS
        }
    ) =
    "telegramBot" := telegramBot
      ~> "telegramChat" := telegramChat
      ~> "telegramHost" := telegramHost
      ~> "toTelegram" := toTelegram
      ~> "apiBuzgibiHost" := apiBuzgibiHost
      ~> "sha256Commit" := sha256Commit
      ~> "cssLink" := cssLink
      ~> "traceLocation" := traceLocation
      ~> "cssFiles" := cssFiles
      ~> "isCaptcha" := isCaptcha
      ~> "apiBuzgibiHostWS" := apiBuzgibiHostWS
      ~> jsonEmptyObject

instance DecodeJson Config where
  decodeJson json = do
    obj <- decodeJson json
    telegramBot <- obj .: "telegramBot"
    telegramChat <- obj .: "telegramChat"
    telegramHost <- obj .: "telegramHost"
    toTelegram <- obj .: "toTelegram"
    apiBuzgibiHost <- obj .: "apiBuzgibiHost"
    sha256Commit <- obj .: "sha256Commit"
    cssLink <- obj .: "cssLink"
    traceLocation <- obj .: "traceLocation"
    cssFiles <- obj .: "cssFiles"
    isCaptcha <- obj .: "isCaptcha"
    apiBuzgibiHostWS <- obj .: "apiBuzgibiHostWS"
    pure $ Config $ { telegramBot, telegramChat, telegramHost, toTelegram, apiBuzgibiHost, sha256Commit, cssLink, traceLocation, cssFiles, isCaptcha, apiBuzgibiHostWS }

setShaCommit :: String -> Config -> Config
setShaCommit x (Config cfg) = Config $ cfg { sha256Commit = x }

setIsCaptcha :: Boolean -> Config -> Config
setIsCaptcha x (Config cfg) = Config $ cfg { isCaptcha = x }

setToTelegram :: Boolean -> Config -> Config
setToTelegram x (Config cfg) = Config $ cfg { toTelegram = x }