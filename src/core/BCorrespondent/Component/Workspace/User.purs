module BCorrespondent.Component.Workspace.User (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Data.Maybe (Maybe (..))

proxy = Proxy :: _ "workspace_user"

loc = "BCorrespondent.Component.Workspace.User"

slot n = HH.slot_ proxy n component

data Action = Initialize

type State = { user :: String }

component =
  H.mkComponent
    { initialState: const { user: mempty }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize }
    } 
    where 
      handleAction Initialize = do
        { user } <- getStore
        for_ user \{jwtUser: {login}} -> 
          H.modify_ _ { user = login }  

render { user }  = 
  HH.div [css "user-container"] 
  [
      HH.span [HPExt.style "padding-right: 20px"] [HH.text user] 
  ,   HH.span_  [HH.i [ css "fa-solid fa-user fa-2xl", HPExt.style "color: #000000; cursor:pointer" ] []]
  ]
    
