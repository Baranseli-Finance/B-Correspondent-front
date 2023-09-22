module System.Time (getTimestamp, timestampToDate) where

import Prelude

import Effect

foreign import getTimestamp :: Effect Int

foreign import timestampToDate :: Int -> Effect String