module BCorrespondent.Component.AppInitFailure (component) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.HTML.Error50x (html)

import Halogen as H
import Halogen.HTML as HH
import Undefined
import Effect.Exception (Error, message)
import Halogen.HTML.Properties.Extended as HPExt

type State = { error :: Error }

component = H.mkComponent { initialState: identity, render: html <<< message <<< _.error, eval: H.mkEval H.defaultEval }