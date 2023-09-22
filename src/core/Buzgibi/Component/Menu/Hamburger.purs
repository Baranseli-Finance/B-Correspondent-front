module Buzgibi.Component.Menu.Hamburger (component, proxy) where

import Prelude

import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Data.Route (Route(..), defUserHistoryParam)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Component.Menu.Navbar (mkItem)
import Buzgibi.Component.Subscription.Logout as Logout

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import DOM.HTML.Indexed.InputType
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Effect.Aff as Aff
import Data.Map as Map
import Halogen.Store.Monad (getStore)
import Data.Maybe (isJust, Maybe(..), fromMaybe)
import Data.Array (concatMap, (:))

proxy = Proxy :: _ "hamburger"

loc = "Buzgibi.Component.HTML.Menu.Hamburger"

data Action = Initialize | LangChange String (Map.Map String String) | Finalize | ShowAuth

type State =
  { route :: Route
  , menu :: Map.Map String String
  , hash :: String
  , isAuth :: Boolean
  , email :: Maybe String
  }

component =
  H.mkComponent
    { initialState:
        \{ route } -> { route: route, menu: Map.empty, hash: mempty, isAuth: false, email: Nothing }
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
        { hash = hash
        , menu = BuzgibiBack.getTranslationMenu translation
        , isAuth = isJust user
        , email = user <#> \{ jwtUser: { email } } -> email
        }
    { menu, hash } <- H.get
    logDebug $ loc <> " ---> " <> show (Map.keys menu)
    logDebug $ loc <> " hash: ---> " <> hash
    Translation.subscribe loc $ \hash translation ->
      handleAction $ LangChange hash $ BuzgibiBack.getTranslationMenu translation
    Logout.subscribe loc $ handleAction ShowAuth
  handleAction (LangChange hash xs) = do
    logDebug $ loc <> " ---> " <> show xs
    logDebug $ loc <> " hash: ---> " <> hash
    H.modify_ _ { hash = hash, menu = xs }
  handleAction Finalize = logDebug $ loc <> " ---> hamburger vanished"
  handleAction ShowAuth = H.modify_ _ { isAuth = false }

-- I piggyback on the following implementation https://codepen.io/fclaw/pen/eYQejgp
render { route, menu, isAuth, email } =
  HH.div [ css "menu-wrap" ]
    [ HH.input [ HPExt.type_ InputCheckbox, css "toggler" ]
    , HH.div [ css "hamburger" ] [ HH.div_ [] ]
    , HH.div [ css "menu" ] [ HH.div_ [ HH.ul_ $ mkUser email : (concatMap (mkItem isAuth route menu addFontStyle) (fromEnum Home .. fromEnum SignIn)) <> [ mkHistoryRef isAuth ] ] ]
    ]
  where
  mkUser (Just x) = HH.li_ [ HH.text x ]
  mkUser Nothing = HH.li_ []
  mkHistoryRef false = HH.li_ []
  mkHistoryRef true = HH.li_ [ HH.a [ css "nav-link", safeHref (UserHistory defUserHistoryParam) ] [ addFontStyle (HH.text (fromMaybe "..." (Map.lookup "history" menu))) ] ]

addFontStyle el = HH.div [ HPExt.style "font-size: 30px; text-align: center" ] [ el ]