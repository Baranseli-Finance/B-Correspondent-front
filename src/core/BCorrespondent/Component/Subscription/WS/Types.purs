module BCorrespondent.Component.Subscription.WS.Types
  ( Resource(..)
  , Transaction
  , TransactionBalancedBook
  , Wallet
  , WalletBalancedBook
  , encodeResource
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

derive instance genericResource :: Generic Resource _

encodeResource :: Resource -> String
encodeResource = unsafeFromForeign <<< genericEncodeEnum {constructorTagTransform: toLower}

transactionUrl = "frontend/user/dashboard/transaction/update"
walletUrl = "frontend/user/dashboard/wallet/update"
withdrawalUrl = "institution/fiat/withdraw/history/item/update"
transactionBalancedBookUrl = "frontend/user/balanced-book/transaction/add"
walletBalancedBookUrl = "frontend/user/balanced-book/wallet/update"