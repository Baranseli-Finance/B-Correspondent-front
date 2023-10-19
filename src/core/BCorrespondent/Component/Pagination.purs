module BCorrespondent.Component.Pagination
  ( Input
  , Output(..)
  , calculateCurrentSegment
  , component
  , proxy
  , slot
  )
  where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, safeHref)
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Data.Route (Route)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array (cons, find, length, snoc)
import Data.Maybe (maybe, Maybe(..))
import Data.Int (toNumber, ceil, rem)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Web.Event.Event (preventDefault)
import Halogen.HTML.Events as HE
import Routing.Duplex (print)


proxy = Proxy :: _ "pagination"

loc = "Buzgibi.Component.Pagination"

slot n { total, perpage, page } = HH.slot proxy n component { total, perpage, page }

type Segment = { xs :: Array Int, next :: Maybe Int }

type State = 
     { currenPage :: Int, 
       total :: Int, 
       perpage :: Int, 
       segment :: Maybe Segment 
     }

data Action = Initialize | Next Int MouseEvent | Receive Input

type Input = { total :: Int, perpage :: Int, page :: Int }

data Output = Page Int

component =
  H.mkComponent
    { initialState: \{ total, perpage, page } ->
        { currenPage: page
        , total: total
        , perpage: perpage
        , segment: Nothing
        }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , receive = Just <<< Receive
        }
    }
  where
  handleAction Initialize = do
    { currenPage, total, perpage } <- H.get
    H.modify_ _ { segment = calculateCurrentSegment currenPage total perpage }
  handleAction (Next curr ev) = do
    logDebug $ loc <> " ---> switch to page " <> show curr
    H.liftEffect $ preventDefault $ toEvent ev
    { total, perpage } <- H.get
    H.modify_ _ { currenPage = curr, segment = calculateCurrentSegment curr total perpage }
    H.raise $ Page curr
  handleAction (Receive input) = do
    logDebug $ loc <> " ---> received from parent " <> show input
    { currenPage, perpage, total } <- H.get
    when (rem input.total perpage == 1) $
      H.modify_ _ { total = input.total, segment = calculateCurrentSegment currenPage input.total perpage }
  
render { segment: Nothing } = HH.div_ []
render { currenPage, segment: Just { xs, next } } =
  HH.nav_
    [ HH.ul [css "pagination-ul"] $
        makeCorner (if currenPage == 1 then Nothing else Just (currenPage - 1)) "Previous"
          `cons`
            ( xs <#> \page ->
                HH.li
                  [ css ( "pagination-ul-li " <> if currenPage == page then "disabled" else mempty) ]
                  [ HH.a [ HPExt.href "#", css (if currenPage == page then "pagination-ul-li-a-active" else "pagination-ul-li-a"), HE.onClick (Next page) ]
                      [ HH.text (show page) ]
                  ]
            )
          `snoc` makeCorner next "Next"
    ]

calculateCurrentSegment :: Int -> Int -> Int -> Maybe Segment
calculateCurrentSegment curr total perPage =
  let
    pages = ceil (toNumber (total / perPage)) + if rem total perPage > 0 then 1 else 0
    go i ys xxs | length ys == 3 = go i [] (xxs `snoc` ys)
    go i ys xxs | i == pages = xxs `snoc` (ys `snoc` i)
    go i ys xxs = go (i + 1) (ys `snoc` i) xxs
    segments = go 1 [] []
    next = if curr < pages then Just (curr + 1) else Nothing
    xsm = flip find segments $ maybe false (const true) <<< find ((==) curr)
  in
    flip map xsm \xs -> { xs: xs, next: next }

makeCorner (Just page) label = HH.li [ css "pagination-ul-li"] [ HH.a [css "pagination-ul-li-a", HPExt.href "#", HE.onClick $ Next page ] [ HH.text label ] ]
makeCorner Nothing label = HH.li [ css "pagination-ul-li" ] [ HH.a [css "pagination-ul-li-a-disabled", HPExt.style "cursor:not-allowed", HPExt.href "#" ] [ HH.text label ] ]