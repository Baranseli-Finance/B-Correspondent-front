module Statistics (sendComponentTime) where

import Prelude

import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Api.Foreign.Request (makeWithResp)
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Data.Config

import Effect
import Halogen as H
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Halogen.Store.Monad (getStore)
import Data.Function.Uncurried (runFn2)
import Foreign (unsafeToForeign)
import Data.Either
import Effect.Exception (message)

sendComponentTime start end component = do
  logDebug "sendComponentTime enter .."
  { config: Config { apiBuzgibiHost, sha256Commit } } <- getStore
  let
    payload =
      "component" := component
        ~> "totalTime" := (end - start)
        ~> jsonEmptyObject
  req <- H.liftEffect $ runFn2 BuzgibiBack.mkLogReq sha256Commit (unsafeToForeign payload)
  resp <- makeWithResp apiBuzgibiHost BuzgibiBack.mkFrontApi $ BuzgibiBack.sendLog req
  case resp of
    Right _ -> logDebug "ok"
    Left err -> logDebug $ "errr- >" <> message err