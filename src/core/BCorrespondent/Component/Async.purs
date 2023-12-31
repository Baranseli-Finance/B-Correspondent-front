module BCorrespondent.Component.Async
  ( Async
  , Level(..)
  , Value
  , component
  , mkException
  , mkOrdinary
  , proxy
  , send
  , withAffjax
  , slot
  ) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Capability.LogMessages (logError, logDebug)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HP
import DOM.HTML.Indexed.ButtonType (ButtonType(ButtonButton))
import Halogen.HTML.Events (onClick)
import Effect.Aff as Aff
import Data.Foldable (for_)
import Concurrent.Channel as Async
import Effect.Exception (Error, message, error)
import Halogen.Store.Monad (getStore)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..), maybe)
import Data.Map as Map
import Data.Functor ((<#>))
import Data.Tuple (Tuple(..))
import Data.List (zip, fromFoldable, length)
import Data.Array ((..))
import System.Time (getTimestamp, timestampToDate, dateToTimestamp)
import Affjax.Web as AX
import Affjax.StatusCode as AX
import Data.Either (Either(..))
import Store.Types (LogLevel(..))

import Undefined

loc = "BCorrespondent.Component.Async"

proxy = Proxy :: _ "async"

slot n = HH.slot_ proxy n component unit

data Action = Close Int | Add Async | Initialize

type AsyncWithTM = { async :: Async, tm :: String }

type State = { xs :: Map.Map Int AsyncWithTM, severity :: LogLevel }

data Level = Warning | Success | Info | Error | Debug

data Value = Exception Error | Ordinary String Level

instance Show Value where
  show (Exception e) = show e
  show (Ordinary s _) = s

type Async = { val :: Value, loc :: Maybe String }

mkException error loc = { val: Exception error, loc: Just loc }
mkOrdinary msg level loc = { val: Ordinary msg level, loc: loc }

component =
  H.mkComponent
    { initialState: const { xs: Map.empty, severity: Dev }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = do
    { async, logLevel } <- getStore
    void $ H.fork $ forever $ do
      H.liftAff $ Aff.delay $ Aff.Milliseconds 1000.0
      val <- H.liftAff $ Async.recv $ _.input async
      for_ val $ \x -> do
        logDebug $ loc <> " ---> async val " <> show x
        handleAction $ Add x

    void $ H.fork $ forever $ do
      H.liftAff $ Aff.delay $ Aff.Milliseconds 1000.0
      {xs} <- H.get
      curr <- H.liftEffect getTimestamp
      let ys = Map.toUnfoldable xs
      for_ (ys :: Array (Tuple Int AsyncWithTM)) 
        \(Tuple idx {tm: d}) -> do
           tm <- H.liftEffect $ dateToTimestamp d 
           when (curr - tm > 30) $
             handleAction (Close idx)

    H.modify_ _ { severity = logLevel }         

  handleAction (Add e@{ val: v }) = do
    let
      isAdded (Exception _) _ = true
      isAdded (Ordinary _ _) Dev = true
      isAdded (Ordinary _ _) Prod = false
    { logLevel } <- getStore
    when (isAdded v logLevel) $ do
      tm <- H.liftEffect $ timestampToDate =<< getTimestamp
      H.modify_ \s -> do
        let m = Map.findMax (_.xs s)
        let
          newXs =
            case m of
              Just { key } ->
                if key + 1 < 10 
                then 
                  Map.insert (key + 1) { async: e, tm: tm } (_.xs s)
                else 
                  Map.insert key { async: e, tm: tm } $ 
                    recalculateIdx $ 
                      Map.delete 1 (_.xs s)
              Nothing -> Map.singleton 1 { async: e, tm: tm }
        s { xs = newXs }
  handleAction (Close idx) = 
    H.modify_ \s -> s { xs = recalculateIdx $ Map.delete idx (_.xs s) }

render { xs, severity } =
  HH.div_ $
    (Map.toUnfoldable xs) <#> \(Tuple k { async: { val, loc }, tm }) ->
      let
        margin = show $ (k - 1) * 15
      in
        HH.div
          [ onClick $ const (Close k)
          , css "alert"
          , HP.style ("top:" <> margin <> "px;cursor: pointer")
          , HP.role "alert"
          ]
          [ HH.p [HP.style "text-align: center", css (mkStyle val) ] 
            [ HH.text (mkMsg val <> if severity == Dev then maybe mempty (\s -> " at " <> s) loc <> ", tm: " <> tm else mempty) ] ]
  where
  mkStyle val =
    case val of
      Exception _ -> "alert-danger"
      Ordinary _ Error -> "alert-danger"
      Ordinary _ Warning -> "alert-warning"
      Ordinary _ Success -> "alert-success"
      Ordinary _ Info -> "alert-info"
      Ordinary _ Debug -> mempty 
  mkMsg val =
    case val of
      Exception err -> message err
      Ordinary msg _ -> msg

recalculateIdx xs =
  let
    valXs = Map.values xs
    idXs = fromFoldable (1 .. length valXs)
  in
    Map.fromFoldable $ zip idXs valXs

send val = do
  logDebug $ loc <> " ---> async start"
  { async } <- getStore
  void $ H.liftAff $ Async.send (_.output async) val
  logDebug $ loc <> " ---> async end"

withAffjax :: forall a. String -> Async.Channel Async Async -> Either AX.Error (AX.Response a) -> (a -> Aff.Aff Unit) -> Aff.Aff Unit
withAffjax loc async (Left e) _ = void $ Async.send (_.output async) $ mkException (error (AX.printError e)) loc
withAffjax _ _ (Right { body, status: (AX.StatusCode 200) }) goWithResp = goWithResp body
withAffjax _ _ (Right { body, status: (AX.StatusCode 202) }) goWithResp = goWithResp body
withAffjax loc async (Right { status: (AX.StatusCode code) }) _ = void $ Async.send (_.output async) $ mkException (error ("server has responded with status " <> show code)) loc