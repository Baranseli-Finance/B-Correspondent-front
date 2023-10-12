module BCorrespondent.Api.Foreign.Frontend
  ( DailyBalanceSheet
  , Direction(..)
  , FrontApi
  , GapItem
  , GapItemTime
  , GapItemUnit
  , Init
  , NextGap
  , Sha
  , _end
  , _hour
  , _min
  , _start
  , fetchTimelineForParticularHour
  , getJwtStatus
  , init
  , initUserDashboardDailyBalanceSheet
  , loadNextGap
  , mkFrontApi
  , printInit
  , shaPred
  )
  where

import Prelude

import BCorrespondent.Api.Foreign.Common

import Data.Function.Uncurried (Fn1, Fn3, runFn3, Fn2, runFn2, runFn4, Fn4)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Maybe (Maybe(..), fromMaybe)
import Undefined
import Foreign (Foreign)
import Effect (Effect)
import Data.Map as Map
import Data.Either (Either)
import Effect.Exception as E
import Data.Array (uncons)
import Data.Lens (lens, Lens)

import Undefined

foreign import data FrontApi :: Type

foreign import mkFrontApi :: Fn1 ApiClient (Effect FrontApi)

type Sha = { key :: String, value :: String }

printSha { key, value } = "{ \"key\": " <> key <> ", \"value\": " <> value <> " }"

shaPred :: String -> Sha -> Boolean
shaPred s {key} = s == key

type Init = 
     { isjwtvalid :: String, 
       shaxs :: Array Sha, 
       level :: String, 
       totelegram :: Boolean,
       telegramchat :: String,
       telegrambot :: String,
       loadcsslocally :: Boolean
     }

printShaxs xs = 
  case uncons xs of 
    Just { head, tail } -> 
      printSha head <> printShaxs tail
    Nothing -> mempty

printInit 
  {isjwtvalid, 
   shaxs, 
   level, 
   totelegram, 
   telegramchat, 
   telegrambot, 
   loadcsslocally
  } = 
  "{ \"isjwtvalid\": " <> isjwtvalid <> 
  ", \"shaxs\": [ " <> printShaxs shaxs <> "] " <>
  ", \"level\": " <> level <>
  ", \"totelegram\": " <> show totelegram <>
  ", \"telegramchat\": " <> telegramchat <>
  ", \"telegrambot\": " <> telegrambot <>
  ", \"loadCssLocally\": " <> show loadcsslocally <> " }"

getJwtStatus :: String -> Maybe JWTStatus
getJwtStatus "valid" = Just Valid
getJwtStatus "invalid" = Just Invalid 
getJwtStatus "skip" = Just Skip
getJwtStatus _ = Nothing

foreign import _init :: Fn3 WithError Json FrontApi (AC.EffectFnAff (Object (Response Init)))

init :: Maybe JWTToken -> FrontApi -> AC.EffectFnAff (Object (Response Init))
init token = runFn3 _init withError (fromMaybe jsonEmptyObject (map encodeJson token))

type GapItemUnit = { status :: String, textualIdent :: String }

type GapItemTime = { hour :: Int, min :: Int }

_hour = lens _.hour $ \el x -> el { hour = x }
_min = lens _.min $ \el x -> el { min = x }

type GapItem = { elements :: Array GapItemUnit, start :: GapItemTime, end :: GapItemTime }

_start = lens _.start $ \el x -> el { start = x }

_end = lens _.end $ \el x -> el { end = x }
  
type DailyBalanceSheet = { gaps :: Array GapItem }

foreign import _initUserDashboardDailyBalanceSheet :: Fn2 WithError FrontApi (AC.EffectFnAff (Object (Response DailyBalanceSheet)))

initUserDashboardDailyBalanceSheet :: FrontApi -> AC.EffectFnAff (Object (Response DailyBalanceSheet))
initUserDashboardDailyBalanceSheet = runFn2 _initUserDashboardDailyBalanceSheet withError


foreign import _loadNextGap :: Fn4 WithError String String FrontApi (AC.EffectFnAff (Object (Response NextGap)))

type NextGap = { gap :: GapItem } 

loadNextGap :: String -> String -> FrontApi -> AC.EffectFnAff (Object (Response NextGap))
loadNextGap = runFn4 _loadNextGap withError

foreign import _fetchTimelineForParticularHour :: Fn4 WithError String String FrontApi (AC.EffectFnAff (Object (Response (Array GapItem))))


data Direction = Backward | Forward

showDirection Backward = "backward"
showDirection Forward = "forward"

fetchTimelineForParticularHour :: Direction -> String -> FrontApi -> AC.EffectFnAff (Object (Response (Array GapItem)))
fetchTimelineForParticularHour direction = runFn4 _fetchTimelineForParticularHour withError (showDirection direction)
