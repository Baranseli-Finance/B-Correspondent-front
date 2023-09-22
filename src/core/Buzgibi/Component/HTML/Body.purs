module Buzgibi.Component.HTML.Body
  ( Body
  , BodyHtml(..)
  , Content
  , Footer
  , Header
  , mkBodyHtml
  ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Buzgibi.Component.HTML.Utils (css)
import Halogen.HTML.Properties.Extended as HPExt

type Content i p = HH.HTML i p
type Header i p = HH.HTML i p
type Body i p = HH.HTML i p
type Footer i p = HH.HTML i p

type BodyHtml =
  { header :: forall i p. Header i p
  , footer :: forall i p. Footer i p
  }

mkBodyHtml { header, footer } route platform width content =
  HH.div_ [ header route platform width, footer, contentWrapper content ]

contentWrapper content = HH.div [ css "body-container" ] [ content ]