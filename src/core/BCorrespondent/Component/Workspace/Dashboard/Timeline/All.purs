module BCorrespondent.Component.Workspace.Dashboard.Timeline.All (slot, proxy, Query (..)) where

import Prelude

import BCorrespondent.Component.Workspace.Dashboard.Transaction (transactionWinOk, transactionWinFailure)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Component.HTML.Utils (whenElem, css, maybeElem)
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseOver, onMouseOut, onClick)
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Effect.Aff as Aff
import Data.Foldable (for_)
import Data.Traversable (for)
import Halogen.Store.Monad (getStore)
import Data.Array (sortWith)
import Control.Alt (alt)
import Data.Maybe (fromMaybe)
import Date.Format (format)
import Data.Lens

import Undefined

proxy = Proxy :: _ "workspace_dashboard_timeline_all"

loc = "BCorrespondent.Component.Workspace.Dashboard.Timeline.All"

slot n = HH.slot_ proxy n component unit

data Query a = Open (Array Back.GapItemUnit) a

data Action = 
        Close 
      | CancelClose
      | FetchInfo String Int Back.GapItemUnitStatus

type State =
     { transactions :: Array Back.GapItemUnit,
       isOpen :: Boolean, 
       forkId :: Maybe H.ForkId,
       isShow :: Boolean,
       transaction :: Maybe Back.NoForeignTransaction
     }

component =
  H.mkComponent
    { initialState: const { transactions: [], isOpen: false, forkId: Nothing, isShow: false, transaction: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction
      , handleQuery = handleQuery 
      }
    }

forkCloseTimer = do
  forkId <- H.fork $ do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 2000.0
    logDebug $ loc <> " ----> close signal is emitted"
    H.modify_ _ 
      { isOpen = false, 
        forkId = Nothing, 
        transactions = [], 
        isShow = false, 
        transaction = Nothing 
      }
  H.modify_ _ { forkId = Just forkId }

handleAction :: forall s . Action -> H.HalogenM State Action s Unit AppM Unit
handleAction Close = forkCloseTimer
handleAction CancelClose = map (_.forkId) H.get >>= flip for_ H.kill
handleAction (FetchInfo textualIdent ident status) 
  | status == Back.Pending || 
    status == Back.GapItemUnitStatusNotResolved = 
     let msg = "transaction for the invoice " <> textualIdent <> " is in process" 
     in Async.send $ Async.mkOrdinary msg Async.Info Nothing
  | otherwise = do
      { config: Config { apiBCorrespondentHost: host }, user, jwtName } <- getStore 
      for_ user \{ token } -> do
        resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
          Back.fetchTrnsaction ident
        let failure e = Async.send $ Async.mkException e loc
        onFailure resp failure \{ success: foreignTr :: Back.ForeignTransaction } -> do
          let res = Back.resolveTransaction foreignTr
          onFailure res failure \tr@{ok, failure} -> do
            oktm <- H.liftEffect $ for ok \x -> do 
              ftm <- format $ x^.Back._timestamp
              pure $ x # Back._timestamp .~ ftm
            failuretm <- H.liftEffect $ for failure \x -> do
              ftm <- format $ x^.Back._timestamp
              pure $ x # Back._timestamp .~ ftm
            logDebug $ loc <> " transaction ---> " <> show tr
            H.modify_ _ { isShow = true, transaction = Just { ok: oktm, failure: failuretm } }

handleQuery :: forall a s . Query a -> H.HalogenM State Action s Unit AppM (Maybe a)
handleQuery (Open xs a) = do 
  forkCloseTimer
  map (const (Just a)) $ H.modify_ _ { transactions = sortWith (_.tm) xs, isOpen = true }

render {isOpen, transactions, isShow, transaction} = 
 whenElem isOpen $
   HH.div_ 
   [
       HH.div
       [ onMouseOut (const Close),
         onMouseOver (const CancelClose),
         css "transaction-all-container"
       ] $ transactions <#> \{textualIdent, ident, status} ->
             let mkColour Back.GapItemUnitStatusNotResolved = "#ffffff"
                 mkColour Back.Pending = "#ffffff"
                 mkColour Back.Ok = "#7ddF3a"
                 mkColour Back.Declined = "#ff5959"
             in HH.span 
                [ onClick (const (FetchInfo textualIdent ident (Back.decodeGapItemUnitStatus status)))
                , css "transaction-all-item"
                , HPExt.style $ 
                    "background-color:" <> 
                    mkColour (Back.decodeGapItemUnitStatus status)
                ] 
                [HH.text textualIdent]
   ,    HH.div [css $ "transaction-all-item-info transform " <> if isShow then "transform-active" else mempty] 
        [maybeElem transaction $ \{ok, failure} ->
          fromMaybe (HH.div_ [HH.text "not transaction to be rendered"]) $
            (ok <#> HH.div_ <<< transactionWinOk) `alt` 
            (failure <#> HH.div_ <<< transactionWinFailure)
        ]
   ]