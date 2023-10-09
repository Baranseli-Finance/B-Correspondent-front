module BCorrespondent.Component.Workspace.User ( slot, Output (..) ) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, whenElem, stylishDiv)
import BCorrespondent.Component.Workspace.User.Menu as Menu

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onMouseOver)
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Data.Maybe (Maybe (..))
import Store (User)

import Undefined

proxy = Proxy :: _ "workspace_user"

loc = "BCorrespondent.Component.Workspace.User"

slot n = HH.slot proxy n component unit

data Action = Initialize | OpenMenu

data Output = OpenDropDownMenu

type State = { user :: String, isMenu :: Boolean }

component =
  H.mkComponent
    { initialState: const { user: mempty, isMenu: false }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize }
    } 
    where 
      handleAction Initialize = do
        { user } <- getStore
        for_ (user :: Maybe User) \{jwtUser: {login}} -> 
          H.modify_ _ { user = login }
      handleAction OpenMenu = H.raise OpenDropDownMenu

render { user } = 
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