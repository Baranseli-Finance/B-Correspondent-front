module BCorrespondent.Component.Timer ( Output(..), slot ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe (Just))
import Type.Proxy (Proxy(..))
import Effect.Aff as Aff
import Control.Monad (void)
import Control.Monad.Rec.Class (forever)

import Undefined

proxy = Proxy :: _ "auth_timer"

loc = "BCorrespondent.Component.Timer"

slot n {interval: interval} = HH.slot proxy n component {interval: interval}

data Action = Initialize | Tick Int

data Output = Emit Int

type State = {interval :: Int }  

component =
  H.mkComponent
    { initialState: \{interval} -> { interval: interval }
    , render: const $ HH.div_ []
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize }
    }
    where 
      handleAction Initialize = do 
        void $ H.fork $ forever $ do
          H.liftAff $ 
            Aff.delay $ 
              Aff.Milliseconds 1000.0
          {interval: val} <- H.get
          when (val - 1 >= 0) $ do
            H.modify_ _ { interval = val - 1 }
            handleAction $ Tick (val - 1)
      handleAction (Tick x) = H.raise $ Emit x