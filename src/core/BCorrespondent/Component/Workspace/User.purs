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

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onMouseOver, onClick)
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Data.Maybe (Maybe (..))
import Store (User)
import AppM (AppM)

import Undefined

proxy = Proxy :: _ "workspace_user"

loc = "BCorrespondent.Component.Workspace.User"

slot n = HH.slot proxy n component unit

data Action = Initialize | OpenMenu | OpenNotification

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
      }
    } 
    where 
      handleAction Initialize = do
        { user } <- getStore
        for_ (user :: Maybe User) \{jwtUser: {login}} -> 
          H.modify_ _ { user = login }
      handleAction OpenMenu = H.raise DropDownMenu
      handleAction OpenNotification = H.raise Notification

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