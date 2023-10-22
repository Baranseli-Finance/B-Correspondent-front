module BCorrespondent.Component.Workspace.Menu (slot, Output (..)) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Halogen.HTML.Events (onClick)
import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Enum.Generic 
       (genericSucc, 
        genericPred, 
        genericCardinality, 
        genericToEnum, 
        genericFromEnum
       )
import Data.Bounded (class Bounded)
import Data.Maybe (fromMaybe)

import Undefined

proxy = Proxy :: _ "workspace_menu"

loc = "BCorrespondent.Component.Workspace.Menu"

slot n = HH.slot proxy n component unit

data Output = Dashboard | BalancedBook | History | Wallet | TechSupport

derive instance genericOutput :: Generic Output _
derive instance eqOutput :: Eq Output
derive instance ordOutput :: Ord Output

instance Enum Output where
  succ = genericSucc
  pred = genericPred

instance Bounded Output where
  top = Dashboard
  bottom = TechSupport

instance BoundedEnum Output where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data Action = OpenDashboard | OpenBalancedBook | OpenHistory | OpenWallet | OpenTechSupport

derive instance genericAction :: Generic Action _
derive instance eqAction :: Eq Action
derive instance ordAction :: Ord Action

instance Enum Action where
  succ = genericSucc
  pred = genericPred

instance Bounded Action where
  top = OpenDashboard
  bottom = OpenTechSupport

instance BoundedEnum Action where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

type State = { selected :: Int }

component =
  H.mkComponent
    { initialState: const { selected: 0 }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }
    where
      handleAction OpenDashboard = 
        H.modify_ _ { selected = fromEnum OpenDashboard } *> 
          H.raise (fromMaybe undefined (toEnum (fromEnum OpenDashboard)))
      handleAction OpenBalancedBook =
        H.modify_ _ { selected = fromEnum OpenBalancedBook } *> 
          H.raise (fromMaybe undefined (toEnum (fromEnum OpenBalancedBook)))
      handleAction OpenHistory = 
        H.modify_ _ { selected = fromEnum OpenHistory } *> 
          H.raise (fromMaybe undefined (toEnum (fromEnum OpenHistory)))
      handleAction OpenWallet = 
        H.modify_ _ { selected = fromEnum OpenWallet } *>  
          H.raise (fromMaybe undefined (toEnum (fromEnum OpenWallet)))
      handleAction OpenTechSupport = 
        H.modify_ _ { selected = fromEnum OpenTechSupport } *>  
          H.raise (fromMaybe undefined (toEnum (fromEnum OpenTechSupport)))

render { selected } = 
  HH.div [css "workspace-menu"]
  [
      HH.div_
      [
          HH.ul 
          [css "ul-menu"] 
          [ 
              HH.li_
              [ 
                HH.div [onClick (const OpenDashboard)] [ HH.i [css "fa fa-home", HPExt.style ("font-size:30px;color:white;" <> mkCursor 0 selected)] [] ]
              ]
          ,   HH.li [HPExt.style "padding-top:30px"]
              [ 
                HH.div [onClick (const OpenBalancedBook)] [ HH.i [css "fa fa-balance-scale", HPExt.style ("font-size:30px;color:white;" <> mkCursor 1 selected)] [] ]
              ]    
          ,   HH.li [HPExt.style "padding-top:30px"]
              [ 
                HH.div [onClick (const OpenHistory)] [ HH.i [css "fa fa-history", HPExt.style ("font-size:30px;color:white;" <> mkCursor 2 selected)] [] ]
              ]
          ,   HH.li [HPExt.style "padding-top:30px"]
              [ 
                HH.div [onClick (const OpenWallet)] [ HH.i [css "fa fa-wallet", HPExt.style ("font-size:30px;color:white;" <> mkCursor 3 selected)] [] ]
              ]
          ,   HH.li [HPExt.style "padding-top:30px"]
              [ 
                HH.div [onClick (const OpenTechSupport)] [ HH.i [css "fa fa-wrench", HPExt.style ("font-size:30px;color:white;" <> mkCursor 4 selected)] [] ]
              ]           
          ]
      ]
  ]

mkCursor :: Int -> Int -> String
mkCursor idx a | idx == a = "cursor: not-allowed"
               | otherwise = "cursor:pointer"
