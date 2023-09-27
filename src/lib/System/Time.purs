module System.Time (getTimestamp, timestampToDate, dateToTimestamp) where

import Prelude

import Effect

foreign import getTimestamp :: Effect Int

foreign import timestampToDate :: Int -> Effect String

foreign import dateToTimestamp :: String -> Effect Int