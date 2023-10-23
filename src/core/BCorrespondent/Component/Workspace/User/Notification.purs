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
import BCorrespondent.Scroll (onDetectVisibile)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onMouseOver, onMouseOut, onScroll)
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Effect.Aff as Aff
import Data.Foldable (for_)
import Store (User)
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe)
import Data.Array ((:))
import Web.Event.Event (Event, target)
import Web.DOM.Element (fromEventTarget, toNode, getAttribute, fromNode)
import Web.DOM.Node (nodeName, childNodes)
import Web.DOM.NodeList (toArray)
import Web.HTML.Common (AttrName (..))
import Web.HTML.HTMLElement as HTML
import Data.Int (fromString)

import Undefined

proxy = Proxy :: _ "workspace_user_notification"

loc = "BCorrespondent.Component.User.Notification"

notificationItemAttr = "notification-item"

slot n = HH.slot_ proxy n component unit

type State = { isOpen :: Boolean, forkId :: Maybe H.ForkId, list :: Array Back.Notification }

data Query a = Open a

data Action = Close | CancelClose | MakeNotificationReadRequest Event

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
handleAction (MakeNotificationReadRequest ev) = do
  let nodem =
        target ev >>= 
          map toNode <<< 
            fromEventTarget
  for_ nodem \parent -> do
    list <- H.liftEffect $ childNodes parent
    nodes <- H.liftEffect $ toArray list
    for_ nodes \div -> do
      list <- H.liftEffect $ childNodes div
      xs <- H.liftEffect $ toArray list
      for_ xs \x -> 
        for_ (fromNode x) \el -> do
          identm <- H.liftEffect $ getAttribute notificationItemAttr el
          for_ identm \ident -> do
            for_ (fromNode div) \parentEl -> do
              isVisible <- H.liftEffect $ onDetectVisibile parentEl el
              when isVisible $ do
                for_ (fromString ident) \i -> do
                  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
                  for_ (user :: Maybe User) \{ token } -> do
                    resp <- Request.makeAuth (Just token) host Back.mkFrontApi $
                      Back.markNotificationRead i
                    let failure = Async.send <<< flip Async.mkException loc  
                    onFailure resp failure \{success: _} -> pure unit

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
        H.modify_ _ { isOpen = true, list = items }

render {isOpen, list} = 
  whenElem isOpen $ 
    HH.div 
    [ onMouseOut (const Close),
      onMouseOver (const CancelClose),
      onScroll MakeNotificationReadRequest,
      css "notification" 
    ] $
     HH.div [HPExt.style "padding-top:10px"] [] :
     (list <#> \{ident, text} -> 
       HH.div_ 
       [ 
           HH.div [css "notification-item", HPExt.attr (AttrName notificationItemAttr) (show ident)] [ HH.text text ]
       ,   HH.div [HPExt.style "padding-top:10px"] [] 
       ])