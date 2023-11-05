module BCorrespondent.Component.Workspace.Dashboard.Gap (Query(..), slot, proxy) where

import Prelude

import BCorrespondent.Api.Foreign.Back (GapItemAmount, Currency (..), decodeCurrency)
import BCorrespondent.Component.HTML.Utils (whenElem)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Functor (($>))
import Data.Array (length)
import Data.Decimal (fromNumber, toFixed)


proxy = Proxy :: _ "workspace_dashboard_gap"

loc = "BCorrespondent.Component.Workspace.Dashboard.Gap"

slot n = HH.slot_ proxy n component unit

data Query a = Open {x :: Maybe Int, y :: Maybe Int, amounts :: Array GapItemAmount} a

type State = 
     { x :: Maybe Int, 
       y :: Maybe Int, 
       amounts :: Array GapItemAmount 
     }

component =
  H.mkComponent
    { initialState: 
      const { x: Nothing, y: Nothing, amounts: [] }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleQuery = handleQuery }
    }
    where 
      handleQuery 
        :: forall a s . Query a
        -> H.HalogenM State Unit s Unit AppM (Maybe a)
      handleQuery (Open query a) =
        H.modify_ _ { x = _.x query, y = _.y query, amounts = _.amounts query } $> Just a

render { x: Nothing, y: Nothing } = HH.div_ []
render { x: Just xCoord, y: Just yCoord, amounts } = 
  let mkStyle = 
        "width:200px;height:100px;\
        \ background-color:white;\
        \ border: solid 1px black; \
        \ position:absolute;left:" <> 
        show (xCoord - 215) <>
        "px;top:" <> 
        show (yCoord - 160) <> "px"
  in whenElem (length amounts > 0) $ 
       HH.div [HPExt.style mkStyle] $ 
         [HH.div_ [HH.text "Total amount"]] <> 
         renderAmounts amounts
render _ = HH.div_ []

renderAmounts xs = 
  flip map xs \{currency, value} -> 
    HH.div_ 
    [  HH.span 
       [HPExt.style "position:relative;font-size:20px"] 
       [HH.text (show (decodeCurrency currency))]
    ,  HH.span [HPExt.style "font-size:20px; padding-left:10px"]
       [HH.text (toFixed 2 (fromNumber value))]
    ]