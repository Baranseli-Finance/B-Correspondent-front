module BCorrespondent.Component.Workspace.User.Menu
  ( Output(..)
  , Query(..)
  , slot
  , proxy
  )
  where

import Prelude

import BCorrespondent.Component.Auth.SignOut as SignOut
import BCorrespondent.Component.HTML.Utils (css, whenElem)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onMouseOver, onMouseOut)
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Data.Functor (($>))
import Effect.Aff as Aff
import Data.Traversable (for_)

import Undefined

proxy = Proxy :: _ "workspace_user_menu"

loc = "BCorrespondent.Component.Workspace.Menu"

slot n = HH.slot proxy n component unit

data Action = HandleChildSignOut SignOut.Output | Close | CancelClose

data Output = LoggedOut

data Query a = Open a

type State = { isOpen :: Boolean, forkId :: Maybe H.ForkId  }

component =
  H.mkComponent
   { initialState: const { isOpen: false, forkId: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery 
      }
   }
   where 
     handleAction (HandleChildSignOut SignOut.LoggedOut) = 
       H.raise LoggedOut
     handleAction Close = do 
       forkId <-  H.fork $ do 
         H.liftAff $ Aff.delay $ Aff.Milliseconds 3000.0 
         H.modify_ _ { isOpen = false }
       H.modify_ _ { forkId = Just forkId }  
     handleAction CancelClose = 
       map (_.forkId) H.get >>= flip for_ H.kill 
     handleQuery 
      :: forall a s . Query a
      -> H.HalogenM State Action s Output AppM (Maybe a)
     handleQuery (Open a) = 
       H.modify_ _ { isOpen = true } $> Just a

render { isOpen } =
  whenElem isOpen $
    HH.div [onMouseOut (const Close), onMouseOver (const CancelClose), css "user-drop-down-menu"] 
    [
        HH.ul [HPExt.style "list-style-type:none;text-align:center"]
        [
            HH.li [HPExt.style "padding-bottom:5px"]
            [ HH.input 
              [ HPExt.style "cursor:pointer", 
                HPExt.type_ HPExt.InputSubmit,
                HPExt.value "reset password"
              ]
            ]
        ,   HH.li_ [SignOut.slot 0 HandleChildSignOut]
        ]
    ]