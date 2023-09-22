module Buzgibi.Component.HTML.Header (html) where

import Prelude

import Buzgibi.Component.Menu.Hamburger as Hamburger
import Buzgibi.Component.Menu.Navbar as Navbar
import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Lang as Lang
import Buzgibi.Component.Async as Async
import Buzgibi.Data.Route (Route)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Store.Types (Platform(..))
import Data.Map as Map

html route pl w =
  HH.div
    [ css "header-container" ]
    [ HH.div [ css "logo-container" ] []
    , HH.div [ css "menu-container" ] [ showMenu route pl w ]
    , HH.div [ css "lang-container" ] [ HH.div_ [ HH.slot_ Lang.proxy unit Lang.component unit ] ]
    , HH.div_ [ HH.slot_ Async.proxy unit Async.component unit ]
    ]

showMenu route Mobile _ = HH.slot_ Hamburger.proxy unit Hamburger.component { route: route }
showMenu route _ w
  | w > 500 = HH.slot_ Navbar.proxy unit Navbar.component { route: route }
  | otherwise = HH.slot_ Hamburger.proxy unit Hamburger.component { route: route }