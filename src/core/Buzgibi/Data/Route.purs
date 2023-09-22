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
module Buzgibi.Data.Route
  ( Route(..)
  , defUserHistoryParam
  , routeCodec
  ) where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root, segment, int, param, default)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Data.Show
import Data.Enum
import Data.Maybe
import Data.Bounded
import Data.Enum.Generic (genericFromEnum, genericToEnum, genericSucc, genericPred, genericCardinality)
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
  | UserSurvey
  | UserHistory String
  | EditSurvey Int
  | EmailConfirmation String
  | Home
  | SignUp
  | SignIn

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance Show Route where
  show Home = "home"
  show Error500 = "500"
  show Error404 = "404"
  show SignUp = "signUp"
  show SignIn = "signIn"
  show UserSurvey = mempty
  show (UserHistory _) = mempty
  show (EditSurvey _) = mempty
  show (EmailConfirmation _) = mempty

instance Enum Route where
  succ Home = Just SignUp
  succ SignUp = Just SignIn
  succ SignIn = Nothing
  succ _ = Nothing
  pred Home = Nothing
  pred SignUp = Just Home
  pred SignIn = Just SignUp
  pred _ = Nothing

instance BoundedEnum Route where
  cardinality = Cardinality 3
  toEnum 0 = Just Home
  toEnum 1 = Just SignUp
  toEnum 2 = Just SignIn
  toEnum _ = Nothing
  fromEnum Home = 0
  fromEnum SignUp = 1
  fromEnum SignIn = 2
  fromEnum _ = undefined

instance Bounded Route where
  top = SignIn
  bottom = Home

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
  , "Error500": "500" / noArgs
  , "Error404": "404" / noArgs
  , "SignUp": "auth" / "signUp" / noArgs
  , "SignIn": "auth" / "signIn" / noArgs
  , "UserSurvey": "user" / "survey" / noArgs
  , "UserHistory": "user" / "history" / default "1" (param "page")
  , "EditSurvey": "user" / "survey" / int (segment)
  , "EmailConfirmation": "auth" / "email" / "confirm" / param "key"
  }

defUserHistoryParam = "1"