module System.Time 
  (getTimestamp, 
   timestampToDate, 
   dateToTimestamp, 
   nowTime, 
   addMinutes
  ) where

import Prelude

import Data.DateTime (Time, time, modifyTime)
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

foreign import _addMinutes :: Int -> Instant -> Effect Instant

addMinutes :: Int -> Time -> Effect Time
addMinutes secs tm = do
   old <- map (fromDateTime <<< modifyTime (const tm) <<< toDateTime) now
   map (time <<< toDateTime) $ _addMinutes secs old