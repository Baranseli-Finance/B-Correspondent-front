module Buzgibi.Page.Home
  ( Action(..)
  , component
  , proxy
  ) where

import Prelude

import Buzgibi.Page.Subscription.WinResize as WinResize
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Data.Route as Route
import Buzgibi.Document.Meta as Meta
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Data.Config
import Buzgibi.Component.HTML.Utils (css, safeHref, whenElem)
import Buzgibi.Component.Subscription.Logout as Logout
import Buzgibi.Page.Home.Html (html)

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
import Statistics (sendComponentTime)

import Undefined

proxy = Proxy :: _ "home"

loc = "Buzgibi.Page.Home"

data Action
  = Initialize
  | WinResize Int
  | LangChange String (Map.Map String String)
  | Finalize
  | Logout

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  , constants :: Map.Map String String
  , hash :: String
  , start :: Int
  , isAuth :: Boolean
  }

component mkBody =
  H.mkComponent
    { initialState: const
        { winWidth: Nothing
        , platform: Nothing
        , constants: Map.empty
        , hash: mempty
        , start: 0
        , isAuth: false
        }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , finalize = pure Finalize
        }
    }
  where
  render { winWidth: Just w, platform: Just p, constants: constants, isAuth } =
    HH.div_ [ mkBody p w (html constants isAuth) ]
  render _ = HH.div_ []
  handleAction Initialize = do
    H.liftEffect $ window >>= document >>= setTitle "Buzgibi | Home"
    { platform, config: Config { apiBuzgibiHost: host }, async, user } <- getStore
    w <- H.liftEffect $ window >>= innerWidth

    tm <- H.liftEffect getTimestamp

    logDebug $ loc <> " component has started at " <> show tm

    void $ initTranslation loc \hash translation -> do
      logDebug $ loc <> " translation ---> " <> show (BuzgibiBack.getTranslationPage translation)
      let
        constants =
          fromMaybe undefined
            $ Map.lookup "home"
            $
              BuzgibiBack.getTranslationPage translation
      H.modify_ _
        { platform = pure platform
        , winWidth = pure w
        , constants = constants
        , hash = hash
        , start = tm
        , isAuth = isJust user
        }

    void $ H.subscribe =<< WinResize.subscribe WinResize

    Meta.set host async $ pure $ BuzgibiBack.MetaPage (show Route.Home)

    Translation.subscribe loc $ \hash translation -> do
      let
        warns =
          fromMaybe undefined
            $ Map.lookup "home"
            $
              BuzgibiBack.getTranslationPage translation
      handleAction $ LangChange hash warns

    Logout.subscribe loc $ handleAction Logout

  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
  handleAction (LangChange hash xs) = H.modify_ _ { constants = xs, hash = hash }
  handleAction Finalize = do
    end <- H.liftEffect getTimestamp
    { start } <- H.get
    sendComponentTime start end loc
  handleAction Logout = H.modify_ _ { isAuth = false }