module Buzgibi.Component.Auth.Email
  ( Action
  , component
  , proxy
  ) where

import Prelude

import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Data.Config (Config(..))

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))
import Halogen.Store.Monad (getStore)
import Data.Traversable (for_)

proxy = Proxy :: _ "auth_email_confirmation"

type State = { isConfirmed :: Maybe Boolean, key :: String }

data Action = Initialize

component =
  H.mkComponent
    { initialState: \{ key } -> { isConfirmed: Nothing, key: key }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = do
    { config: Config { apiBuzgibiHost }, user } <- getStore
    for_ user \{ token } -> do
      { key } <- H.get
      resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.confirmEmail { key: key }
      withError resp \{ success: res } -> H.modify_ _ { isConfirmed = Just res }

render { isConfirmed: Nothing } = HH.div_ [ HH.text "confiormation is being processed..." ]
render { isConfirmed: Just true } = HH.div_ [ HH.text "the email is confirmed" ]
render { isConfirmed: Just false } = HH.div_ [ HH.text "we cannot confirm the email" ]