module BCorrespondent.Component.Workspace.Dashboard.Gap (Query(..), slot, proxy) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Data.Functor (($>))

proxy = Proxy :: _ "workspace_dashboard_gap"

loc = "BCorrespondent.Component.Workspace.Dashboard.Gap"

slot n = HH.slot_ proxy n component unit

data Query a = Open {x :: Maybe Int, y :: Maybe Int} a

type State = { x :: Maybe Int, y :: Maybe Int }

component =
  H.mkComponent
    { initialState: const { x: Nothing, y: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleQuery = handleQuery }
    }
    where 
      handleQuery 
        :: forall a s . Query a
        -> H.HalogenM State Unit s Unit AppM (Maybe a)
      handleQuery (Open coord a) = 
        H.modify_ _ { x = _.x coord, y = _.y coord } $> Just a

render { x: Nothing, y: Nothing } = HH.div_ []
render { x: Just xCoord, y: Just yCoord } = 
  let mkStyle = 
        "width:200px;height:100px;background-color:black;position:absolute;left:" <> 
        show (xCoord - 30) <> 
        "px;top:" <> 
        show (yCoord - 150) <> "px"
  in HH.div [HPExt.style mkStyle] [HH.text "gap"]
render _ = HH.div_ []