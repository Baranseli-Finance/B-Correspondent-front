module Web.Socket.ReadyState
  ( ReadyState(..)
  , fromEnumReadyState
  , toEnumReadyState
  ) where

import Prelude
import Data.Enum (Cardinality(..), class BoundedEnum, defaultPred, defaultSucc, class Enum)
import Data.Maybe (Maybe(..))

data ReadyState
  = Connecting
  | Open
  | Closing
  | Closed

derive instance eqReadyState :: Eq ReadyState
derive instance ordReadyState :: Ord ReadyState

instance boundedReadyState :: Bounded ReadyState where
  bottom = Connecting
  top = Closed

instance enumReadyState :: Enum ReadyState where
  succ = defaultSucc toEnumReadyState fromEnumReadyState
  pred = defaultPred toEnumReadyState fromEnumReadyState

instance boundedEnumReadyState :: BoundedEnum ReadyState where
  cardinality = Cardinality 4
  toEnum = toEnumReadyState
  fromEnum = fromEnumReadyState

instance showReadyState :: Show ReadyState where
  show Connecting = "Connecting"
  show Open = "Open"
  show Closing = "Closing"
  show Closed = "Closed"

toEnumReadyState :: Int -> Maybe ReadyState
toEnumReadyState 0 = Just Connecting
toEnumReadyState 1 = Just Open
toEnumReadyState 2 = Just Closing
toEnumReadyState 3 = Just Closed
toEnumReadyState _ = Nothing

fromEnumReadyState :: ReadyState -> Int
fromEnumReadyState Connecting = 0
fromEnumReadyState Open = 1
fromEnumReadyState Closing = 2
fromEnumReadyState Closed = 3