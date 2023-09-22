module File.Blob (downloadBlob) where

import Prelude

import Web.File.Blob (Blob)

foreign import downloadBlob :: Blob -> String -> Unit

