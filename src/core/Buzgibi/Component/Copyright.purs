module Buzgibi.Component.Copyright
  ( component
  , proxy
  , render
  ) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack

import Halogen as H
import Halogen.HTML as HH
import Undefined
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "copyright"

loc = "TTHouse.Component.Copyright"

data Action = Initialize | LangChange String String

type State = { copyright :: String, hash :: String }

component =
  H.mkComponent
    { initialState: const { copyright: mempty, hash: mempty }
    , render: render
    , eval: H.mkEval H.defaultEval
        { initialize = pure Initialize
        , handleAction = handleAction
        }
    }
  where
  handleAction Initialize = do
    void $ initTranslation loc \hash translation ->
      H.modify_ _ { copyright = BuzgibiBack.getTranslationCopyright translation }
    Translation.subscribe loc $ \hash translation ->
      handleAction $ LangChange hash $ BuzgibiBack.getTranslationCopyright translation
  handleAction (LangChange _ new) = H.modify_ _ { copyright = new }

render { copyright } = HH.div [ css "copyright-plaque" ] [ HH.text copyright ]