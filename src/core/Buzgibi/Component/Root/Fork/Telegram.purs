module Buzgibi.Component.Root.Fork.Telegram (fork, init) where

import Prelude

import Buzgibi.Component.Async (withAffjax)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Data.Config

import Halogen as H
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Concurrent.Channel as Async
import Halogen.Store.Monad (getStore)
import Affjax.Web as AX
import Affjax.ResponseFormat as AX
import Effect.Now as Now
import Affjax.RequestBody as AXB
import Data.FormURLEncoded as AXD
import Data.Tuple (Tuple(..))
import Data.Foldable (for_)

loc = "Buzgibi.Component.Root.Fork.Telegram:"

fork { mkBody, url_msg } = do
  logDebug $ (loc <> "fork") <> " ---> telegram fork start .."
  { telegramVar, async } <- getStore
  void $ H.fork $ forever $ H.liftAff do
    Aff.delay $ Aff.Milliseconds 2000.0
    res <- Async.recv $ _.input telegramVar
    for_ res \msg -> do
      resp <- AX.post AX.json url_msg (pure (mkBody msg))
      withAffjax loc async resp $ const (pure unit)

init = do
  logDebug $ (loc <> "init") <> " ---> telegram init start"
  { config: Config { telegramHost, telegramBot, telegramChat, toTelegram } } <- getStore
  let url_msg = telegramHost <> telegramBot <> "/sendMessage"
  let
    mkBody msg =
      AXB.FormURLEncoded $
        AXD.FormURLEncoded
          [ Tuple "chat_id" (pure telegramChat)
          , Tuple "text" (pure ("`" <> msg <> "`"))
          , Tuple "parse_mode" (pure "markdown")
          ]
  logDebug $ (loc <> "init") <> " ---> telegram init end"
  pure { mkBody, url_msg }