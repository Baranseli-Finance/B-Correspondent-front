module Buzgibi.Page.Auth
  ( Action(..)
  , component
  , proxy_sign_in
  , proxy_sign_up
  ) where

import Prelude

import Buzgibi.Component.HTML.Body as Body
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Document.Meta as Meta
import Buzgibi.Page.Subscription.WinResize as WinResize
import Buzgibi.Data.Config
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Data.Route as Route

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))
import Store.Types (Platform)
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Halogen.Store.Monad (getStore)
import System.Time (getTimestamp)
import Statistics (sendComponentTime)

proxy_sign_in = Proxy :: _ "auth_container_sign_in"
proxy_sign_up = Proxy :: _ "auth_container_sign_up"

loc = "Buzgibi.Page.Auth"

data Action
  = Initialize
  | WinResize Int
  | Finalize

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  , hash :: String
  , start :: Int
  , route :: Route.Route
  , title :: String
  }

component mkBody authComp =
  H.mkComponent
    { initialState: \{ route, title } ->
        { winWidth: Nothing
        , platform: Nothing
        , hash: mempty :: String
        , start: 0
        , route: route
        , title: title
        }
    , render: render mkBody authComp
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , finalize = pure Finalize
        }
    }
  where
  handleAction Initialize = do
    { route, title } <- H.get
    H.liftEffect $ window >>= document >>= setTitle ("Buzgibi | " <> title)
    { platform, config: Config { apiBuzgibiHost: host }, async } <- getStore
    w <- H.liftEffect $ window >>= innerWidth

    tm <- H.liftEffect getTimestamp

    logDebug $ loc <> " component has started at " <> show tm

    void $ H.subscribe =<< WinResize.subscribe WinResize

    Meta.set host async $ pure $ BuzgibiBack.MetaPage (show route)

    H.modify_ _ { platform = pure platform, winWidth = pure w }
  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
  handleAction Finalize = do
    end <- H.liftEffect getTimestamp
    { start } <- H.get
    sendComponentTime start end loc

render mkBody authComp { winWidth: Just w, platform: Just p } = HH.div_ [ mkBody p w authComp ]
render _ _ _ = HH.div_ []