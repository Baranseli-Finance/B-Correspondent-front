module Buzgibi.Component.Lang (component, proxy) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Capability.LogMessages (logDebug, class LogMessages)
import Buzgibi.Capability.Now (class Now)
import Buzgibi.Component.Lang.Data
import Buzgibi.Data.Route as Route
import Buzgibi.Component.Async as Async

import Halogen.Store.Monad (class MonadStore)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Array ((..))
import Halogen.HTML.Properties.Extended as HPExt
import Data.Maybe (fromMaybe, Maybe(..), isNothing)
import Undefined
import Halogen.HTML.Events as HE
import Data.Foldable (for_)
import Halogen.Store.Monad (getStore)
import Store (Store)
import Data.Enum (toEnum, fromEnum)
import Effect.Aff.Class
import Store (Action)
import Effect.AVar as Async
import AppM (AppM)

proxy = Proxy :: _ "lang"

loc = "Buzgibi.Component.Lang"

component :: forall q i o. MonadStore Action Store AppM => H.Component q i o AppM
component =
  H.mkComponent
    { initialState: const { lang: 0 }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = do
    { langVar } <- getStore
    res <- H.liftEffect $ Async.tryRead langVar
    for_ res \lang -> H.modify_ _ { lang = fromEnum lang }
  handleAction (Notify idx) = do
    let langm = toEnum idx
    logDebug $ show langm
    for_ langm $ \lang -> do
      { lang: curr } <- H.get
      H.modify_ _ { lang = fromEnum lang }
      { langVar } <- getStore
      void $ H.liftEffect $ Async.tryTake langVar
      res <- H.liftEffect $ Async.tryPut lang langVar
      if res then
        logDebug $
          loc
            <> " app lang has been updated from "
            <> show (toLang curr)
            <> " to "
            <>
              show lang
      else logDebug $ loc <> " app lang hasn't been updated"

render { lang } =
  HH.div [ css "header-lang-wrapper" ]
    [ HH.select
        [ css "form-select form-select-sm"
        , HE.onSelectedIndexChange Notify
        , HPExt.selectedIndex lang
        ]
        ( flip map (fromEnum Eng .. fromEnum Turk) $ \ident ->
            let
              str = show <<< fromMaybe undefined <<< (toEnum :: Int -> Maybe Lang)
            in
              HH.option [ HPExt.value (str ident) ] [ HH.text (str ident) ]
        )
    ]

toLang :: Int -> Lang
toLang = fromMaybe undefined <<< toEnum