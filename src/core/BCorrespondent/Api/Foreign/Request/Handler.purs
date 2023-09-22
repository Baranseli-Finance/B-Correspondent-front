module BCorrespondent.Api.Foreign.Request.Handler
  ( onFailure
  , withAffjax
  , withError
  ) where

import Prelude

import BCorrespondent.Data.Route (Route(Error500))
import BCorrespondent.Capability.LogMessages (class LogMessages)
import BCorrespondent.Capability.Now (class Now)
import BCorrespondent.Capability.Navigate (class Navigate)
import BCorrespondent.Capability.Navigate (navigate)
import BCorrespondent.Capability.LogMessages (logError)

import Data.Either (Either(..))
import Store (Action(WriteError))
import Halogen.Store.Monad (updateStore)
import Effect.Exception (Error, error)
import Halogen.Query.HalogenM (HalogenM)
import Effect.Aff.Class (class MonadAff)
import Halogen.Store.Monad (class MonadStore)
import Store (Action, Store)
import Affjax.Web as AX
import Affjax.StatusCode as AX

withError
  :: forall m a s xs ys o
   . LogMessages m
  => Now m
  => MonadAff m
  => MonadStore Action Store m
  => Navigate m
  => Either Error a
  -> (a -> HalogenM s xs ys o m Unit)
  -> HalogenM s xs ys o m Unit
withError (Right x) success = success x
withError (Left e) _ = logError (show e) *> updateStore (WriteError e) *> navigate Error500

onFailure
  :: forall m a s xs ys o
   . LogMessages m
  => Now m
  => MonadAff m
  => Either Error a
  -> (Error -> HalogenM s xs ys o m Unit)
  -> (a -> HalogenM s xs ys o m Unit)
  -> HalogenM s xs ys o m Unit
onFailure (Right x) _ success = success x
onFailure (Left e) failure _ = failure e

withAffjax
  :: forall m a s xs ys o
   . LogMessages m
  => Now m
  => MonadAff m
  => MonadStore Action Store m
  => Navigate m
  => String
  -> Either AX.Error (AX.Response a)
  -> (a -> HalogenM s xs ys o m Unit)
  -> HalogenM s xs ys o m Unit
withAffjax loc (Left e) _ = logError (AX.printError e) *> updateStore (WriteError (error (AX.printError e))) *> navigate Error500
withAffjax _ (Right { body, status: (AX.StatusCode 200) }) goWithResp = goWithResp body
withAffjax _ (Right { body, status: (AX.StatusCode 202) }) goWithResp = goWithResp body
withAffjax loc (Right { status: (AX.StatusCode code) }) _ = logError (show code) *> updateStore (WriteError (error (show code))) *> navigate Error500