module BCorrespondent.Component.Subscription.WS.Types (Transaction) where

type Transaction = 
     { status :: String,
       dayOfYear :: Int,
       hour :: Int,
       min :: Int,
       textualIdent :: String
     }

