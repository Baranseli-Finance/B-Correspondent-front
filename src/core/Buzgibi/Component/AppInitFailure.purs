module Buzgibi.Component.AppInitFailure (component) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.HTML.Error50x (html)

import Halogen as H
import Halogen.HTML as HH
import Undefined
import Effect.Exception (Error, message)
import Halogen.HTML.Properties.Extended as HPExt

type State = { error :: Error }

component = H.mkComponent { initialState: identity, render: html <<< message <<< _.error, eval: H.mkEval H.defaultEval }