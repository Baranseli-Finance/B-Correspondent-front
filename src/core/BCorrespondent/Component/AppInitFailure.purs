module BCorrespondent.Component.AppInitFailure (component) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Component.HTML.Error50x (html)

import Halogen as H
import Halogen.HTML as HH
import Undefined
import Effect.Exception (Error, message)
import Halogen.HTML.Properties.Extended as HPExt
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)

type State = { error :: Error }

data Action = Initialize

component = 
  H.mkComponent 
  { initialState: identity, 
    render: html <<< message <<< _.error,
    eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = pure Initialize
    }
  }
  where
    handleAction Initialize = do
      H.liftEffect $ 
        window >>=
          document >>= 
            setTitle 
              "BCorrespondent | \
              \ Techincal failure"