module BCorrespondent.Component.Subscription.WS.Types
  ( Resource(..)
  , Transaction
  , encodeResource
  , transactionUrl
  )
  where

import Prelude

import Foreign (Foreign, unsafeFromForeign)
import Data.Generic.Rep (class Generic)
import Foreign.Enum (genericEncodeEnum)
import Data.String (toLower)

type Transaction = 
     { status :: Foreign,
       dayOfYear :: Int,
       hour :: Int,
       min :: Int,
       textualIdent :: String
     }

data Resource = Transaction | Wallet

derive instance genericResource :: Generic Resource _

encodeResource :: Resource -> String
encodeResource = unsafeFromForeign <<< genericEncodeEnum {constructorTagTransform: toLower}

transactionUrl = "frontend/user/dashboard/daily-balance-sheet/transaction/update"