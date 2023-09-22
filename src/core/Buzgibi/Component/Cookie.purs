module Buzgibi.Component.Cookie (component, proxy) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Cookie.Foreign as Cookie
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Data.Config

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HP
import DOM.HTML.Indexed.ButtonType (ButtonType(ButtonButton))
import Halogen.HTML.Events (onClick)
import Undefined
import Data.Maybe (isJust)
import Data.Function.Uncurried (runFn1)
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)
import Data.Either (Either)
import Effect.Exception (Error)
import Data.Traversable (sequence, for)
import Data.Maybe (Maybe)

proxy = Proxy :: _ "cookie"

data Action = Close | Initialize

type State = { close :: Boolean }

component =
  H.mkComponent
    { initialState: const { close: false }
    , render: render
    , eval: H.mkEval H.defaultEval
        { initialize = pure Initialize
        , handleAction = \action ->
            case action of
              Initialize -> do
                { cookies } <- getStore
                res :: Maybe (Array String) <- map sequence $ for cookies $ H.liftEffect <<< Cookie.get
                when (isJust res) $ H.modify_ _ { close = true }
              Close -> do
                { config: Config { apiBuzgibiHost: host } } <- getStore
                resp :: Either Error (BuzgibiBack.Success (Array BuzgibiBack.Cookie)) <-
                  Request.make host BuzgibiBack.mkFrontApi $ BuzgibiBack.getCookies
                withError resp \{ success: xs } -> do
                  for_ xs \cookie -> do
                    H.liftEffect $ Cookie.set cookie
                    logDebug $ "cookie " <> show cookie <> " has been set"
                  H.modify_ _ { close = true }
        }
    }

render { close } =
  HH.div
    [ HP.id "cb-cookie-banner"
    , css "alert alert-dark text-center mb-0"
    , HP.role "alert"
    , if close then HP.style "display: none" else HP.style "display: block"
    ]
    [ HH.b_ [ HH.text "This website uses cookies to ensure you get the best experience on our website." ]
    , HH.button [ onClick (const Close), HP.type_ ButtonButton, css "btn btn-dark btn-sm ms-3" ] [ HH.text "I agree" ]
    ]