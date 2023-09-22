module BCorrespondent.Page.Home
  ( Action(..)
  , component
  , proxy
  ) where

import Prelude

import BCorrespondent.Page.Subscription.WinResize as WinResize
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Data.Route as Route
import BCorrespondent.Data.Config
import BCorrespondent.Component.HTML.Utils (css, safeHref, whenElem)
import BCorrespondent.Page.Home.Html (html)

import Halogen.HTML.Properties.Extended as HPExt
import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Action(WriteError))
import Store.Types (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import Data.Map as Map
import System.Time (getTimestamp)

import Undefined

proxy = Proxy :: _ "home"

loc = "BCorrespondent.Page.Home"

data Action
  = Initialize
  | WinResize Int

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  }

component =
  H.mkComponent
    { initialState: const
        { winWidth: Nothing
        , platform: Nothing
        }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  render { winWidth: Just _, platform: Just _ } = HH.div_ [ html ]
  render _ = HH.div_ []
  handleAction Initialize = do
    H.liftEffect $ window >>= document >>= setTitle "BCorrespondent | Home"
    { platform } <- getStore
    w <- H.liftEffect $ window >>= innerWidth

    H.modify_ _ { platform = pure platform, winWidth = pure w }

    void $ H.subscribe =<< WinResize.subscribe WinResize

  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }