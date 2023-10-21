module BCorrespondent.Component.Workspace.TechSupport (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.FileUploader as FileUploader 
import BCorrespondent.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Type.Proxy (Proxy(..))
import Data.Traversable (for_)
import Data.Maybe (Maybe (Just))
import Data.Array ((:))

proxy = Proxy :: _ "workspace_tech_support"

loc = "BCorrespondent.Component.Workspace.TechSupport"

slot n = HH.slot_ proxy n component unit

data Action = 
        HandleChildFileUploader FileUploader.Output 
      | SumbitIssueRequest Event

type State = { files :: Array Int }

component =
  H.mkComponent
    { initialState: const { files: [] }
    , render: const render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }


handleAction (HandleChildFileUploader (FileUploader.FileIds xs)) = 
  for_ xs \{ident, title} -> do
    let msg = "file " <> title <> " has been uploaded"
    Async.send $ Async.mkOrdinary msg Async.Success (Just loc)
    H.modify_ \s -> s { files = ident : _.files s }
handleAction (SumbitIssueRequest ev) = H.liftEffect $ preventDefault ev

render = 
  HH.div [css "issue-container"] 
  [ 
      HH.div_ [HH.text "Techical Support"]
  ,   HH.div [HPExt.style "padding-top:10px"] [HH.textarea [HPExt.cols 80, HPExt.rows 25, HPExt.style "resize:none"]]
  ,   HH.div [HPExt.style "padding-top:10px"] [ FileUploader.slot 0 "issue" HandleChildFileUploader ]
  ,   HH.div [HPExt.style "padding-top:10px"]
      [
        HH.form [ HE.onSubmit SumbitIssueRequest ]
        [
            HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "sumbit", HPExt.style "cursor: pointer" ]
        ]
      ]
  ]
