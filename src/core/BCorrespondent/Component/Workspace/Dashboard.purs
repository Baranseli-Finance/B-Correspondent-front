module BCorrespondent.Component.Workspace.Dashboard (slot) where

import Prelude

import BCorrespondent.Data.Config (Config(..))
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Api.Foreign.Request as Request
import BCorrespondent.Api.Foreign.Back as Back
import BCorrespondent.Api.Foreign.Request.Handler (onFailure)

import Halogen.Store.Monad (getStore)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes as Svg
import Type.Proxy (Proxy(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe (..))
import Effect.Exception (message)
import Data.Int (toNumber)

import Undefined

proxy = Proxy :: _ "workspace_dashboard"

loc = "BCorrespondent.Component.Workspace.Dashboard"

slot n = HH.slot_ proxy n component unit

data Action = Initialize

type State = { error :: Maybe String, gaps :: Array Back.GapItem }

component =
  H.mkComponent
    { initialState: const { error: Nothing, gaps: [] }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }
    where 
      handleAction Initialize = do
        { config: Config { apiBCorrespondentHost: host }, user } <- getStore
        for_ user \{ token } -> do
          resp <- Request.makeAuth (Just token) host Back.mkFrontApi $ 
            Back.initUserDashboardDailyBalanceSheet
          let failure e = H.modify_ _ { error = Just $ "cannot load component: " <> message e }
          onFailure resp failure \{ success: {gaps} } -> pure unit

render {error: Just e} = HH.text e
render {error: Nothing} = Svg.svg [Svg.width (toNumber 1400), Svg.height (toNumber 900)] [ Svg.text [] [HH.text "dashboard"] ]