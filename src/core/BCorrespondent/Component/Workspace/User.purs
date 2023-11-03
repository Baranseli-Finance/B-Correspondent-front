module BCorrespondent.Component.Workspace.User
  ( Output(..)
  , Query(..)
  , proxy
  , slot
  )
  where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, whenElem, stylishDiv)
import BCorrespondent.Component.Workspace.User.Menu as Menu
import BCorrespondent.Component.Subscription.WS as WS
import BCorrespondent.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onMouseOver, onClick)
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Data.Maybe (Maybe (..))
import Store (User, WS)
import AppM (AppM)
import Web.Socket as WS
import Effect.AVar as Async

import Undefined

proxy = Proxy :: _ "workspace_user"

loc = "BCorrespondent.Component.Workspace.User"

slot n = HH.slot proxy n component unit

data Action = 
        Initialize
      | Finalize  
      | OpenMenu 
      | OpenNotification 
      | IncreaseNotificationCount

data Output = DropDownMenu | Notification

type State = 
     { user :: String, 
       isMenu :: Boolean, 
       amountNotification ::  Int 
     }

data Query a = AmountNotification Int a

component =
  H.mkComponent
    { initialState: const 
      { user: mempty, isMenu: false, amountNotification: 0 }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      ,  handleQuery = handleQuery
      , finalize = pure Finalize
      }
    } 
    where 
      handleAction Initialize = do
        { user } <- getStore
        for_ (user :: Maybe User) \{jwtUser: {login}} -> 
          H.modify_ _ { user = login }
        void $ H.fork $
          WS.subscribe loc WS.notificationUrl (Just (WS.encodeResource WS.Notification)) $
            handleAction <<< const IncreaseNotificationCount <<< _.success
      handleAction Finalize = do
        { wsVar } <- getStore
        wsm <- H.liftEffect $ Async.tryTake (wsVar :: Async.AVar (Array WS))
        for_ wsm \xs ->
          for_ xs \{ ws, forkId } -> do
            H.kill forkId
            H.liftEffect $ WS.close ws
            logDebug $ loc <> " ---> ws has been killed"         
      handleAction OpenMenu = H.raise DropDownMenu
      handleAction OpenNotification = H.raise Notification
      handleAction IncreaseNotificationCount = 
        H.modify_ \s -> s { amountNotification = 1 + _.amountNotification s }

handleQuery :: forall a s . Query a -> H.HalogenM State Action s Output AppM (Maybe a)
handleQuery (AmountNotification c a) = map (const (Just a)) $ H.modify_ _ { amountNotification = c }

render { user, amountNotification: c } =
  HH.div_
  [
      HH.div [css "user-container"] 
      [
          HH.div [ css "avatar"] 
          [ HH.div [css "user-login"] [HH.text user]
          , stylishDiv 
            [onMouseOver $ const OpenMenu, 
            css "user-icon font", 
            HPExt.style "cursor:pointer"
            ]
          ]
      ]
  ,   whenElem (c /= 0) $ HH.div [css "unread-notifs"] [HH.text (show c)]
  ,   let bellStyle = if c == 0 then "fa fa-bell fa-lg" else "bell-animate fa fa-bell fa-lg"
      in  HH.div [css "bell", onClick $ const OpenNotification] [HH.i [css bellStyle] []]
  ]