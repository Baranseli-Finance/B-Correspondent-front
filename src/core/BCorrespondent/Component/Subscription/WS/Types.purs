module BCorrespondent.Component.Subscription.WS.Types
  ( Resource(..)
  , Transaction
  , Wallet
  , encodeResource
  , transactionUrl
  , walletUrl
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

type Wallet = 
     { ident :: Int,
       amount :: Number
     }

data Resource = Transaction | Wallet

derive instance genericResource :: Generic Resource _

encodeResource :: Resource -> String
encodeResource = unsafeFromForeign <<< genericEncodeEnum {constructorTagTransform: toLower}

transactionUrl = "frontend/user/dashboard/daily-balance-sheet/transaction/update"
walletUrl = "frontend/user/dashboard/wallet/update"