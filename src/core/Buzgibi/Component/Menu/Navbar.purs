module Buzgibi.Component.Menu.Navbar (component, proxy, mkItem) where

import Prelude

import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Data.Route (Route(..))
import Buzgibi.Component.Lang.Data (Lang(..))
import Buzgibi.Capability.LogMessages (logDebug, logError)
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Component.Subscription.Logout as Logout
import Buzgibi.Component.Auth.User as Auth.User

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import Type.Proxy (Proxy(..))
import Effect.Aff as Aff
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Halogen.Store.Monad (getStore)
import Data.Array (concatMap, snoc)

import Undefined

proxy = Proxy :: _ "navbar"

loc = "Buzgibi.Component.HTML.Menu.Navbar"

data Action = Initialize | LangChange String (Map.Map String String) | Finalize | ShowAuth

type State =
  { route :: Route
  , menu :: Map.Map String String
  , hash :: String
  , isAuth :: Boolean
  }

component =
  H.mkComponent
    { initialState:
        \{ route } -> { route: route, menu: Map.empty, hash: mempty, isAuth: false }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , finalize = pure Finalize
        }
    }
  where
  handleAction Initialize = do
    { user } <- getStore
    void $ initTranslation loc \hash translation ->
      H.modify_ _
        { menu = BuzgibiBack.getTranslationMenu translation
        , hash = hash
        , isAuth = isJust user
        }
    { menu, hash } <- H.get
    logDebug $ loc <> " menu: ---> " <> show (Map.keys menu)
    logDebug $ loc <> " hash: ---> " <> hash
    Translation.subscribe loc $ \hash translation ->
      handleAction $ LangChange hash $ BuzgibiBack.getTranslationMenu translation
    Logout.subscribe loc $ handleAction ShowAuth
  handleAction (LangChange hash xs) = do
    logDebug $ loc <> " ---> " <> show xs
    logDebug $ loc <> " hash: ---> " <> hash
    H.modify_ _ { hash = hash, menu = xs }
  handleAction Finalize = logDebug $ loc <> " ---> nav bar vanished"
  handleAction ShowAuth = H.modify_ _ { isAuth = false }

render { route, menu, isAuth } =
  HH.nav [ css "navbar navbar-expand-lg navbar-light" ]
    [ HH.ul [ css "navbar-nav mr-auto" ]
        ( (concatMap (mkItem isAuth route menu addFontStyle) (fromEnum Home .. fromEnum SignIn)) `snoc`
            HH.slot_ Auth.User.proxy unit Auth.User.component unit
        )
    ]

mkItem _ _ xs _ _ | Map.isEmpty xs = [ HH.li_ [ HH.text "..." ] ]
mkItem isAuth route xs applyStyle idx =
  if (mkRoute idx == SignUp || mkRoute idx == SignIn) && isAuth then []
  else
    [ HH.li [ css "nav-item" ]
        [ HH.a
            [ css "nav-link"
            , safeHref (mkRoute idx)
            , isDisabled (mkRoute idx == route)
            ]
            [ el ]
        ]
    ]
  where
  mkRoute = fromMaybe undefined <<< (toEnum :: Int -> Maybe Route)
  isDisabled true = HPExt.style "cursor: not-allowed;"
  isDisabled false = HPExt.style mempty
  title = fromMaybe (show (mkRoute idx)) $ Map.lookup (show (mkRoute idx)) xs
  el = applyStyle $ HH.text title

addFontStyle el = HH.div [ HPExt.style "font-size:20px; text-align:center" ] [ el ]
