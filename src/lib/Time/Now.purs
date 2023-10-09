module Time.Now ( nowTime ) where

import Prelude

import Data.DateTime (Time, time)
import Data.DateTime.Instant (Instant, toDateTime)
import Effect (Effect)

-- | Gets an `Instant` value for the date and time according to the current
-- | machine’s clock.
foreign import now :: Effect Instant

-- | Gets the time according to the current machine’s clock.
nowTime :: Effect Time
nowTime = time <<< toDateTime <$> now