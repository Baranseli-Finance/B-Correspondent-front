module BCorrespondent.Component.Workspace.User.Notification
  ( Query(..)
  , proxy
  , slot
  )
  where

import Prelude

import BCorrespondent.Component.HTML.Utils (whenElem, css)
import BCorrespondent.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onMouseOver, onMouseOut)
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Effect.Aff as Aff
import Data.Foldable (for_)

proxy = Proxy :: _ "workspace_user_notification"

loc = "BCorrespondent.Component.User.Notification"

slot n = HH.slot_ proxy n component unit

type State = { isOpen :: Boolean, forkId :: Maybe H.ForkId }

data Query a = Open a

data Action = Close | CancelClose

component =
  H.mkComponent
    { initialState: const { isOpen: false, forkId: Nothing }
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
    H.modify_ _ { isOpen = false, forkId = Nothing }
  H.modify_ _ { forkId = Just forkId }

handleAction Close = forkCloseTimer
handleAction CancelClose = map (_.forkId) H.get >>= flip for_ H.kill

handleQuery
  :: forall a s . Query a
  -> H.HalogenM State Action s Unit AppM (Maybe a)
handleQuery (Open a) = do
  forkCloseTimer 
  map (const (Just a)) $ 
    H.modify_ _ { isOpen = true }

render {isOpen} = 
  whenElem isOpen $ 
    HH.div 
    [ onMouseOut (const Close),
      onMouseOver (const CancelClose), 
      css "notification" 
    ] 
    [ HH.text "there'll be notifications" ]
