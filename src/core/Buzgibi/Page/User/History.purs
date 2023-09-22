module Buzgibi.Page.User.History
  ( Action(..)
  , component
  , proxy
  ) where

import Prelude

import Buzgibi.Data.Config
import Buzgibi.Page.Subscription.WinResize as WinResize
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Data.Route as Route
import Buzgibi.Document.Meta as Meta
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Component.List as List
import Buzgibi.Component.Subscription.Logout as Logout

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store.Types (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import System.Time (getTimestamp)
import Statistics (sendComponentTime)
import Data.Int (fromString)

proxy = Proxy :: _ "user_history"

loc = "Buzgibi.Page.User.History"

data Action = Initialize | WinResize Int | Finalize | ToHome

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  , start :: Int
  , page :: Int
  }

component mkBody =
  H.mkComponent
    { initialState: \{ page } ->
        { winWidth: Nothing
        , platform: Nothing
        , start: 0
        , page: fromMaybe 1 (fromString page)
        }
    , render: render mkBody
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , finalize = pure Finalize
        }
    }
  where
  handleAction Initialize = do
    { user } <- getStore
    when (isNothing user) $ navigate Route.Home
    H.liftEffect $ window >>= document >>= setTitle "Buzgibi | History"
    { platform, config: Config { apiBuzgibiHost: host }, async } <- getStore
    w <- H.liftEffect $ window >>= innerWidth

    tm <- H.liftEffect getTimestamp

    logDebug $ loc <> " component has started at " <> show tm

    { page } <- H.get

    H.modify_ _ { platform = pure platform, winWidth = pure w, start = tm }

    void $ H.subscribe =<< WinResize.subscribe WinResize

    Meta.set host async $ pure $ BuzgibiBack.MetaPage (show (Route.UserHistory mempty))

    Logout.subscribe loc $ handleAction ToHome

  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
  handleAction Finalize = do
    end <- H.liftEffect getTimestamp
    { start } <- H.get
    sendComponentTime start end loc
  handleAction ToHome = navigate Route.Home

render mkBody { winWidth: Just w, platform: Just p, page } = mkBody p w (HH.slot_ List.proxy unit List.component { page: page })
render _ _ = HH.div_ []