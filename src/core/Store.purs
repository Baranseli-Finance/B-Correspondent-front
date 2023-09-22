-- | A global state containing information useful to most components in the
-- | application. If you've ever used Redux, this should look familiar.
-- | Components can read, write, and subscribe to this central state, which is
-- | called a "store" by convention.
module Store
  ( Action(..)
  , EditSurvey
  , Store(..)
  , User
  , WS
  , initAppStore
  , printStore
  , reduce
  ) where

import Prelude

import Buzgibi.Data.Config (Config)
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Capability.LogMessages (logError, logDebug)
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Component.Lang.Data (Lang, Recipients)
import Buzgibi.Data.Route (Route)
import Buzgibi.Component.Async as Async
import Store.Types

import Halogen as H
import Data.Maybe (Maybe(..))
import Effect.Exception (Error, message)
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
import Cache as Cache
import Concurrent.Channel as Async
import Crypto.Jwt (JwtUser)
import Effect.Ref (Ref)
import Web.Socket as WS

type User = { jwtUser :: JwtUser, token :: BuzgibiBack.JWTToken }

type EditSurvey = { survey :: Int, voice :: Int }

type WS = { ws :: WS.WebSocket, forkId :: H.ForkId }

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
  , init :: BuzgibiBack.Init
  , cache :: Cache.Cache
  , async :: Async.Channel Async.Async Async.Async
  , cookies :: Array String
  , langVar :: AVar Lang
  , telegramVar :: Async.Channel String String
  , logLevel :: LogLevel
  , user :: Maybe User
  , isLogoutVar :: AVar Unit
  --- for soem unexplicable reason the child-parent communicaton won't work
  -- I didn't manage to get this feature working
  -- ad-hoc approach is to employ Avar for passing something to a parent
  --- reference to docs: https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html#output-messages
  , paginationVar :: Ref (Maybe Int)
  , isTest :: Boolean
  , editSurvey :: AVar EditSurvey
  , wsVar :: AVar (Array WS)
  }

printStore store =
  "{ config: " <> stringify (encodeJson (_.config store))
    <> ", error: "
    <> fromMaybe mempty (map message (_.error store))
    <> ", platform: "
    <> show (_.platform store)
    <> ", init: "
    <> show (_.init store)
    <> ", cache: "
    <> show (_.cache store)
    <> ", async: <AVar> "
    <> ", cookies: "
    <> show (_.cookies store)
    <> ", langVar: <AVar>"
    <> ", telegramVar: <AVar>"
    <> ", logLevel: "
    <> show (_.logLevel store)
    <> ", user:  "
    <> show (_.user store)
    <> ", isLogoutVa: <AVar> "
    <>
      ", paginationVar: <Ref>"
    <> ", isTest: "
    <> show (_.isTest store)
    <> ", editSurvey: <AVar> "
    <> ", wsVar: <AVar>"

-- | Ordinarily we'd write an initialStore function, but in our case we construct
-- | all three values in our initial store during app initialization. For that
-- | reason, you can see the store get constructed in the `Main` module.

-- | Next, we'll define a data type that represents state updates to our store.
-- | The log level and base URL should remain constant, but we'll need to be
-- | able to set the current user.
data Action
  = WriteError Error
  | WriteTranslationToCache BuzgibiBack.Translation String
  | UpdateJwtUser (Maybe User)

-- | Finally, we'll map this action to a state update in a function called a
-- | 'reducer'. If you're curious to learn more, see the `halogen-store`
-- | documentation!
reduce :: Store -> Action -> Store
reduce store (WriteError err) = store { error = Just err }
reduce store (WriteTranslationToCache x hash) = store { cache = Cache.writeTranslation x hash (_.cache store) }
reduce store (UpdateJwtUser user) = store { user = user }

initAppStore :: String -> Maybe BuzgibiBack.JWTToken -> Aff (Either Excep.Error BuzgibiBack.Init)
initAppStore host token = map (map _.success) $ Request.make host BuzgibiBack.mkFrontApi $ BuzgibiBack.init token