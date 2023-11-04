-- | A global state containing information useful to most components in the
-- | application. If you've ever used Redux, this should look familiar.
-- | Components can read, write, and subscribe to this central state, which is
-- | called a "store" by convention.
module Store
  ( Action(..)
  , Store(..)
  , User
  , WS
  , initAppStore
  , printStore
  , reduce
  ) where

import Prelude

import BCorrespondent.Data.Config (Config)
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Capability.LogMessages (logError, logDebug)
import BCorrespondent.Data.Route (Route)
import BCorrespondent.Component.Async as Async
import BCorrespondent.Api.Foreign.Request as Request

import Store.Types
import Halogen as H
import Data.Maybe (Maybe(..))
import Effect.Exception (Error, message, error)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Maybe
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Traversable (for)
import Effect.Class (liftEffect)
import Control.Monad.Error.Class (throwError)
import Data.Either
import Effect.Exception as Excep
import Data.Bifunctor (lmap)
import Undefined
import Data.Function.Uncurried (runFn0)
import Data.Traversable (sequence)
import Effect.AVar (AVar)
import Data.Map as Map
import Concurrent.Channel as Async
import Crypto.Jwt (JwtUser)
import Effect.Ref (Ref)
import Web.Socket as WS
import Data.Date (Date)

type User = { jwtUser :: JwtUser, token :: Back.JWTToken }

type WS = { ws :: WS.WebSocket, forkId :: H.ForkId, component :: String }

-- | We can now construct our central state which will be available to all
-- | components (if they opt-in).
-- |
-- | First, we'll use a `LogLevel` flag to indicate whether we'd like to log
-- | everything (`Dev`) or only critical messages (`Prod`). Next, we'll maintain
-- | a configurable base URL. We'll also hold on to the currently-logged-in user.
type Store =
  { config :: Config
  , error :: Maybe Error
  , platform :: Platform
  , async :: Async.Channel Async.Async Async.Async
  , telegramVar :: Async.Channel String String
  , logLevel :: LogLevel
  , user :: Maybe User
  , wsVar :: AVar (Array WS)
    -- browser fingerprint
  , browserFp :: String
  , jwtName :: String
  , now :: Date
  , since :: Back.InvoiceSince
  }

printStore store =
  "{ config: " <> stringify (encodeJson (_.config store))
    <> ", error: "
    <> fromMaybe mempty (map message (_.error store))
    <> ", platform: "
    <> show (_.platform store)
    <> ", async: <AVar> "
    <> ", telegramVar: <AVar>"
    <> ", logLevel: "
    <> show (_.logLevel store)
    <> ", user: <user>"
    <> ", wsVar: <AVar>"
    <> ", browserFp: " <> (_.browserFp store)
    <> ", jwtName: " <> (_.jwtName store)
    <> ", now: " <> show (_.now store)
    <> ", since: " <> show (_.since store) <> " }"

-- | Ordinarily we'd write an initialStore function, but in our case we construct
-- | all three values in our initial store during app initialization. For that
-- | reason, you can see the store get constructed in the `Main` module.

-- | Next, we'll define a data type that represents state updates to our store.
-- | The log level and base URL should remain constant, but we'll need to be
-- | able to set the current user.
data Action
  = WriteError Error
  | UpdateJwtUser (Maybe User)

-- | Finally, we'll map this action to a state update in a function called a
-- | 'reducer'. If you're curious to learn more, see the `halogen-store`
-- | documentation!
reduce :: Store -> Action -> Store
reduce store (WriteError err) = store { error = Just err }
reduce store (UpdateJwtUser user) = store { user = user }

initAppStore :: String -> Maybe Back.JWTToken -> Aff (Either Excep.Error Back.Init)
initAppStore host token = map (map _.success) $ Request.make host Back.mkFrontApi $ Back.init token