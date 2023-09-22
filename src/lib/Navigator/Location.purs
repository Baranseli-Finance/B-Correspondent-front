module Navigator.Location (getLocation, Coordinates) where

import Prelude

import Data.Function.Uncurried (Fn0, runFn0)
import Effect.Aff.Compat as AC
import Effect.Aff (Aff)
import Data.Either (Either)
import Effect.Aff (try)
import Effect.Exception (message)
import Data.Bifunctor (lmap)

type Coordinates = { latitude :: Number, longitude :: Number }

foreign import _getLocation :: Fn0 (AC.EffectFnAff Coordinates)

getLocation :: Aff (Either String Coordinates)
getLocation = map (lmap message) $ try $ AC.fromEffectFnAff $ runFn0 _getLocation