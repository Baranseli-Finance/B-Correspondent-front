module Web.Browser (getBrowserIdentifier) where

import Prelude

import Effect.Aff.Compat as AC
import Effect.Aff (try, Aff)
import Effect (Effect)
import Data.Either (either)
import Data.Function.Uncurried (Fn0, runFn0)
import Control.Monad.Error.Class (throwError)

foreign import _getBrowserIdentifier :: Fn0 (AC.EffectFnAff String)

getBrowserIdentifier :: Aff String
getBrowserIdentifier = do 
  res <- try $ AC.fromEffectFnAff $ runFn0 _getBrowserIdentifier
  either throwError pure res