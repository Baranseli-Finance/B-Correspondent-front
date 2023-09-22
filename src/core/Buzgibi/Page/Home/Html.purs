module Buzgibi.Page.Home.Html (html) where

import Prelude

import Buzgibi.Data.Route as Route
import Buzgibi.Component.HTML.Utils (css, safeHref)

import Halogen.HTML.Properties.Extended as HPExt
import Halogen.Html.Raw.Render as H
import Data.Maybe
import Halogen as H
import Halogen.HTML as HH
import Data.Map as Map
import Undefined

html constants isAuth =
  HH.div_
    [ HH.div [ css "home-headline" ]
        [ HH.h1_
            [ HH.span [ css "text-gradient", HPExt.style "color: linear-gradient(102deg, #5D3DF8 0%, #DF3ECB 100%)" ]
                [ HH.text "Conduct surveys" ]
            , HH.br_
            , HH.text "without hassle"
            , HH.br_
            , HH.span [ css "text-gradient" ] [ HH.text "Fast and Easy" ]
            ]
        ]
    , HH.div [ css "make-survey-button-container" ]
        [ HH.div [ css "make-survey-button" ]
            [ if isAuth then
                HH.a [ safeHref Route.UserSurvey ] [ HH.text $ fromMaybe undefined $ Map.lookup "makeSurvey" constants ]
              else
                HH.a [ safeHref Route.SignUp ] [ HH.text $ fromMaybe undefined $ Map.lookup "signUp" constants ]
            ]
        ]
    ]

