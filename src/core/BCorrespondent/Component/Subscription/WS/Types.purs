module BCorrespondent.Component.Subscription.WS.Types
  ( Resource(..)
  , Transaction
  , TransactionBalancedBook
  , Wallet
  , WalletBalancedBook
  , encodeResource
  , notificationUrl
  , transactionBalancedBookUrl
  , transactionUrl
  , walletBalancedBookUrl
  , walletUrl
  , withdrawalUrl
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
       textualIdent :: String,
       tm :: String,
       ident :: Int
     }

type Wallet = 
     { ident :: Int,
       amount :: Number
     }

type TransactionBalancedBook =
      { from :: Int, 
        to :: Int, 
        amount :: Number, 
        currency :: Foreign,
        dow :: Int,
        institutionTitle :: String
      }

type WalletBalancedBook = 
     { ident :: Int,
        institution :: Int,
        amount :: Number 
     }

data Resource = 
       Transaction 
     | Wallet 
     | Withdrawal 
     | BalancedBookTransaction
     | BalancedBookWallet
     | Notification

derive instance genericResource :: Generic Resource _

encodeResource :: Resource -> String
encodeResource = unsafeFromForeign <<< genericEncodeEnum {constructorTagTransform: toLower}

transactionUrl = "ws/frontend/user/dashboard/transaction/update"
walletUrl = "ws/frontend/user/dashboard/wallet/update"
withdrawalUrl = "ws/institution/fiat/withdraw/history/item/update"
transactionBalancedBookUrl = "ws/frontend/user/balanced-book/transaction/add"
walletBalancedBookUrl = "ws/frontend/user/balanced-book/wallet/update"
notificationUrl = "ws/frontend/user/notification"