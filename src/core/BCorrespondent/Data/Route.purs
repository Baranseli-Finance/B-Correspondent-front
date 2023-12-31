-- | We can represent valid locations in our application with a simple sum type. This will cause
-- | any invalid routes to fail at compile-time.
-- |
-- | But since the browser represents locations with strings, we'll also need a way to write our
-- | `Route` type to a string and parse strings into valid `Route` values. It's tedious and error-
-- | prone to maintain separate printing and parsing functions which can fall out of sync with
-- | another, and even worse to write them manually. Fortunately, the `routing-duplex` library will
-- | help us write a bi-directional codec which solves both problems.
-- |
-- | For more information about the library and to read the tutorial, see:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
module BCorrespondent.Data.Route
  ( Route(..)
  , routeCodec
  ) where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root, param)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Data.Show
import Data.Maybe
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Undefined

-- | We'll represent routes in our application with a simple sum type. As the application grows,
-- | you might want to swap this out with an extensible sum type with `Variant` and have several
-- | sub-sections. For our small MVP this type will work just fine and will prevent us from trying
-- | to send users to non-existent routes.
data Route
  = Error500
  | Error404
  | Home
  | NewPassword String

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance Show Route where
  show Home = "home"
  show (NewPassword _) = "new password"
  show Error500 = "500"
  show Error404 = "404"

instance DecodeJson Route where
  decodeJson = genericDecodeJson

-- | Next, we'll define a bidirectional codec for our route parsing. Our single codec will handle
-- | both parsing browser locations and serializing our data type to a browser location. We'll skip
-- | the boilerplate of separate encoding and decoding functions, and we'll ensure our parsing and
-- | printing is always in sync.
-- |
-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "NewPassword": "auth" / "password" / "reset" / param "key"
  , "Error500": "500" / noArgs
  , "Error404": "404" / noArgs
  }