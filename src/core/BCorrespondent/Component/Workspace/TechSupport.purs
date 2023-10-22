module BCorrespondent.Component.Workspace.TechSupport (slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.FileUploader as FileUploader 
import BCorrespondent.Component.Async as Async
import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)
import BCorrespondent.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Type.Proxy (Proxy(..))
import Data.Traversable (for_)
import Data.Maybe (Maybe (Just))
import Data.Array ((:), concat)
import Store (User)
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe (Nothing))
import Data.String (length)

proxy = Proxy :: _ "workspace_tech_support"

loc = "BCorrespondent.Component.Workspace.TechSupport"

slot n = HH.slot_ proxy n component unit

data Action = 
        HandleChildFileUploader FileUploader.Output 
      | SumbitIssueRequest Event
      | WriteIssue String

type State = { files :: Array Int, issue :: String }

component =
  H.mkComponent
    { initialState: const { files: mempty, issue: (mempty :: String) }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
    }


handleAction (HandleChildFileUploader (FileUploader.FileIds xs)) = 
  for_ xs \{ident, title} -> do
    let msg = "file " <> title <> " has been uploaded"
    Async.send $ Async.mkOrdinary msg Async.Success (Just loc)
    H.modify_ \s -> s { files = ident : _.files s }
handleAction (WriteIssue value) = H.modify_ _ { issue = value }
handleAction (SumbitIssueRequest ev) = do 
  H.liftEffect $ preventDefault ev
  { config: Config { apiBCorrespondentHost: host }, user } <- getStore
  for_ (user :: Maybe User) \{ token } -> do
    {files, issue} <- H.get
    when (length issue > 0) $ do
      logDebug $ loc <> " files ----> " <> show files
      resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
        Back.submitIssue {files: files, description: issue}
      let failure e = do 
            finilise
            Async.send $ Async.mkException e loc
      onFailure resp failure \{success: _} -> do
        let msg = "issue has been submitted"
        finilise
        Async.send $ Async.mkOrdinary msg Async.Success Nothing

finilise = do 
  H.modify_ _ { issue = mempty, files = [] }
  H.tell FileUploader.proxy 0 FileUploader.EraseFile

render {issue} = 
  HH.div [css "issue-container"] 
  [ 
      HH.div_ [HH.text "Techical Support"]
  ,   HH.div 
      [HPExt.style "padding-top:10px"] 
      [HH.textarea [ HE.onValueInput WriteIssue, HPExt.cols 80, HPExt.rows 25, HPExt.style "resize:none", HPExt.value issue ]]
  ,   HH.div [HPExt.style "padding-top:10px"] 
      [ FileUploader.slot 0 "issue" HandleChildFileUploader ]
  ,   HH.div [HPExt.style "padding-top:10px"]
      [
        HH.form [ HE.onSubmit SumbitIssueRequest ]
        [
            HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "sumbit", HPExt.style "cursor: pointer" ]
        ]
      ]
  ]
