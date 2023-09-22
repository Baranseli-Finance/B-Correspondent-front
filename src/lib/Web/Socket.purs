module Web.Socket
  ( Protocol(..)
  , WebSocket
  , close
  , create
  , readState
  , send
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn1, runFn2, runFn1)
import Data.Unit
import Effect
import Partial.Unsafe (unsafePartial)
import Web.Socket.ReadyState (toEnumReadyState, ReadyState)
import Data.Maybe (fromJust)
import Effect.Aff.Compat as AC
import Data.Either
import Web.File.Blob (Blob, fromString)
import Data.MediaType (MediaType(..))

newtype Protocol = Protocol String

foreign import data WebSocket :: Type

foreign import _create :: Fn2 String (Array Protocol) (Effect WebSocket)

foreign import _readState :: Fn1 WebSocket (Effect Int)

create :: String -> Array Protocol -> Effect WebSocket
create = runFn2 _create

readState :: WebSocket -> Effect ReadyState
readState ws = do
  st <- runFn1 _readState ws
  pure $ unsafePartial $ fromJust $ toEnumReadyState st

foreign import _send :: Fn2 WebSocket Blob (Effect Unit)

foreign import _unsafeStringify :: forall a. a -> String

send :: forall a. WebSocket -> a -> Effect Unit
send ws o = runFn2 _send ws $ fromString (_unsafeStringify o) (MediaType "application/json")

foreign import _close :: WebSocket -> Effect Unit

close :: WebSocket -> Effect Unit
close = _close