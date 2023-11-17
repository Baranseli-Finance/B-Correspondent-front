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
import Data.Maybe (Maybe (..), maybe)
import Effect.Aff as Aff
import Data.Foldable (for_)
import Store (User)
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe)
import Data.Array ((:), null, nub, take, last)
import Web.Event.Event (Event, target, preventDefault)
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

type State = 
     { isOpen :: Boolean, 
       forkId :: Maybe H.ForkId, 
       list :: Array Back.Notification, 
       isScroll :: Boolean,
       isReadList :: Array Int,
       lastElement :: Int
     }

data Query a = Open a

data Action = 
        Close 
      | CancelClose 
      | MakeNotificationReadRequest Event
      | FetchNextBatch Int

component =
  H.mkComponent
    { initialState: const 
      { isOpen: false, 
        forkId: Nothing, 
        list: [],
        isScroll: false,
        isReadList: [],
        lastElement: 0
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
    dumpNotifications
    H.modify_ _ { isOpen = false, forkId = Nothing, list = [] }
  H.modify_ _ { forkId = Just forkId }

handleAction :: forall q . Action -> H.HalogenM State Action q Unit AppM Unit
handleAction Close = forkCloseTimer
handleAction CancelClose = map (_.forkId) H.get >>= flip for_ H.kill
handleAction (MakeNotificationReadRequest ev) = do
  {isScroll, lastElement} <- H.get
  logDebug $ loc <> " ----> last element: " <> show lastElement
  when (not isScroll) $ do
    H.modify_ _ { isScroll = true }
    logDebug $ loc <> " ----> scroll event is evoked"
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
              for_ (fromNode parent) \parentEl -> do
                isVisible <- H.liftEffect $ onDetectVisibile parentEl el
                when isVisible $ do
                  for_ (fromString ident) \i -> do
                    H.modify_ \s -> s { isReadList = nub $ i : _.isReadList s }
                    when (i == lastElement) $ handleAction (FetchNextBatch i)
    H.modify_ _ { isScroll = false }
handleAction (FetchNextBatch idx) = do
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do
    resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
      Back.fetchNotifications $ { from: idx }
    let failure = Async.send <<< flip Async.mkException loc
    onFailure resp failure \{success: {items}} -> do
      logDebug $ loc <> " notification ----> " <> show (items :: Array Back.Notification)
      let lastEl = maybe 0 _.ident $ last items
      H.modify_ \s -> s { list = _.list s <> items, lastElement = lastEl }

dumpNotifications = do
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do
    {isReadList} <- H.get
    when (not null isReadList) $ do
      logDebug $ loc <> " ----> notification are about to be sent for isRead: " <> show isReadList
      resp <- Request.makeAuth (Just token) host Back.mkFrontApi $
        Back.markNotificationRead isReadList
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
        Back.fetchNotifications { from: 0 }
      let failure = Async.send <<< flip Async.mkException loc
      onFailure resp failure \{success: {items}} -> do
        logDebug $ loc <> " notification ----> " <> show (items :: Array Back.Notification)
        let lastEl = maybe 0 _.ident $ last items
        H.modify_ _ { isOpen = true, list = items, isReadList = take 6 $ map _.ident items, lastElement = lastEl }

render {isOpen, list} = 
  whenElem isOpen $ 
    HH.div
    [ onMouseOut (const Close),
      onMouseOver (const CancelClose),
      onScroll MakeNotificationReadRequest,
      css $ if null list then "no-notification" else "notification" 
    ] $
      if null list then
        [renderNoNotification]
      else  
        HH.div [HPExt.style "padding-top:10px"] [] :
        (list <#> \{ident, text} -> 
          HH.div [HPExt.style "margin-right:10px;margin-left:10px"]
          [ 
              HH.div 
              [css "notification-item",
                HPExt.attr (AttrName notificationItemAttr) (show ident)] 
              [ HH.text text ]
          ,   HH.div [HPExt.style "padding-top:10px"] [] 
          ])

renderNoNotification = HH.text "there are no new notifications"