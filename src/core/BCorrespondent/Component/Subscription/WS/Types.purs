module BCorrespondent.Component.Subscription.WS.Types (Transaction) where

import Foreign (Foreign)

type Transaction = 
     { status :: Foreign,
       dayOfYear :: Int,
       hour :: Int,
       min :: Int,
       textualIdent :: String
     }

