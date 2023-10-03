module BCorrespondent.Component.FileUploader
  ( Output(..)
  , Query(..)
  , slot
  , proxy
  )
  where

import Prelude

import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (withError)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.Input (RefLabel(..))
import Halogen.HTML.Properties.Extended as HPExt
import Web.DOM.Internal.Types (Element)
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Web.File.File (File)
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe (..))
import Data.Foldable (for_)
import Data.Array ((:))
import Data.Traversable (traverse_)
import AppM (AppM)

import Undefined

proxy = Proxy :: _ "file_uploader"

loc = "BCorrespondent.Component.FileUploader"

slot n s = HH.slot proxy n component s

foreign import removeFile :: Element -> Effect Unit

data Action = Upload (Array File)

data Output = FileIds (Array Int)

type State = { ids :: Array Int, bucket :: String }

data Query a = EraseFile a

component =
  H.mkComponent
  { initialState: \bucket -> { ids: [], bucket: bucket }
  , render: const $ render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction,
      handleQuery = handleQuery
    }
  }
  where
    handleAction (Upload fs) = do
      { config: Config { apiBCorrespondentHost: host }, user } <- getStore
      {bucket} <- H.get
      for_ user \{token} -> do
        for_ fs \file -> do
          resp <- Request.makeAuth (Just token) host Back.mkFileApi $
            Back.upload bucket file
          withError resp \{ success: ident :: Int } ->
            H.modify_ \s -> s { ids = ident : (_.ids s) }
      map (FileIds <<< _.ids) H.get >>= H.raise
    handleQuery 
      :: forall a . Query a 
      -> H.HalogenM State Action () Output AppM (Maybe a)
    handleQuery (EraseFile a) = do 
      H.getRef (RefLabel "file") >>=
        traverse_ (H.liftEffect <<< removeFile)
      map (const (Just a)) $ H.modify_ _ { ids = [] }

render = 
  HH.input
    [ HPExt.type_ HPExt.InputFile
    , HE.onFileUpload Upload
    , HPExt.multiple true
    , HPExt.ref $ RefLabel "file"
    ]