module Buzgibi.Page.Error.Page500 (component, proxy) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Effect.Exception (message)
import Data.Foldable (for_)
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route (Route(Error404))
import Data.Maybe (isNothing)
import Web.HTML.Window (document)
import Web.HTML (window)
import Web.HTML.HTMLElement (toElement)
import Web.DOM.Element (setClassName)
import Web.HTML.HTMLDocument (body)
import Halogen.HTML.Properties.Extended as HPExt

proxy = Proxy :: _ "error500"

type State = { msg :: String }

data Action = Initialize

component =
  H.mkComponent
    { initialState: const { msg: mempty }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = do
    H.liftEffect $ do
      win <- window
      doc <- document win
      bodym <- body doc
      for_ bodym \body ->
        "body-error" `setClassName` toElement body

    { error } <- getStore
    when (isNothing error) $ navigate Error404
    for_ error \e -> H.modify_ _ { msg = message e }

render { msg } =
  HH.section [ css "centered" ]
    [ HH.h1_ [ HH.text "Internal Server Error or Network failure" ]
    , HH.div [ css "container" ] [ HH.span [ HPExt.style "font-size:20px; color:red;" ] [ HH.text msg ] ]
    ]