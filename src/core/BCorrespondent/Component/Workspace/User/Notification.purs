module BCorrespondent.Component.Workspace.User.Notification
  ( Query(..)
  , proxy
  , slot
  )
  where

import Prelude

import BCorrespondent.Component.HTML.Utils (whenElem, css)
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Component.Async as Async
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onMouseOver, onMouseOut)
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Effect.Aff as Aff
import Data.Foldable (for_)
import Store (User)
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe)
import Data.Array ((:))

proxy = Proxy :: _ "workspace_user_notification"

loc = "BCorrespondent.Component.User.Notification"

slot n = HH.slot_ proxy n component unit

type State = { isOpen :: Boolean, forkId :: Maybe H.ForkId, list :: Array String }

data Query a = Open a

data Action = Close | CancelClose

component =
  H.mkComponent
    { initialState: 
      const 
      { isOpen: false, 
        forkId: Nothing, 
        list: [] 
      }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
    }

forkCloseTimer = do
  forkId <- H.fork $ do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 3000.0
    logDebug $ loc <> " ----> close signal is emitted" 
    H.modify_ _ { isOpen = false, forkId = Nothing, list = [] }
  H.modify_ _ { forkId = Just forkId }

handleAction Close = forkCloseTimer
handleAction CancelClose = map (_.forkId) H.get >>= flip for_ H.kill


handleQuery
  :: forall a s . Query a
  -> H.HalogenM State Action s Unit AppM (Maybe a)
handleQuery (Open a) = do
  forkCloseTimer
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  map (const (Just a)) $ 
    for_ (user :: Maybe User) \{ token } -> do
      resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
        Back.fetchNotifications
      let failure = Async.send <<< flip Async.mkException loc
      onFailure resp failure \{success: {items}} -> do
        logDebug $ loc <> " notification ----> " <> show (items :: Array Back.Notification)
        H.modify_ _ { isOpen = true, list = map _.text items }

render {isOpen, list} = 
  whenElem isOpen $ 
    HH.div 
    [ onMouseOut (const Close),
      onMouseOver (const CancelClose), 
      css "notification" 
    ] $
     HH.div [HPExt.style "padding-top:10px"] [] : 
     (list <#> \text -> 
       HH.div_ 
       [ 
           HH.div [css "notification-item"] [ HH.text text ]
       ,   HH.div [HPExt.style "padding-top:10px"] [] 
       ])