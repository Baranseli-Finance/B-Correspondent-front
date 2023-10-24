module System.Time
  ( addDays
  , addMinutes
  , dateToTimestamp
  , getTimestamp
  , getTimezone
  , nowDate
  , nowTime
  , timestampToDate
  )
  where

import Prelude

import Data.DateTime (Time, Date, time, date, modifyTime)
import Data.DateTime.Instant (Instant, toDateTime, fromDateTime)
import Effect (Effect)

foreign import getTimestamp :: Effect Int

foreign import timestampToDate :: Int -> Effect String

foreign import dateToTimestamp :: String -> Effect Int

-- | Gets an `Instant` value for the date and time according to the current
-- | machine’s clock.
foreign import now :: Effect Instant

-- | Gets the time according to the current machine’s clock.
nowTime :: Effect Time
nowTime = time <<< toDateTime <$> now

nowDate :: Effect Date
nowDate = date <<< toDateTime <$> now

foreign import _addMinutes :: Int -> Instant -> Effect Instant

addMinutes :: Int -> Time -> Effect Time
addMinutes secs tm = do
   old <- map (fromDateTime <<< modifyTime (const tm) <<< toDateTime) now
   map (time <<< toDateTime) $ _addMinutes secs old

foreign import getTimezone :: Effect Int

foreign import addDays :: Int -> String -> Effect String


