module BCorrespondent.Api.Foreign.Institution
  ( Balance
  , ForeignBalance
  , ForeignWithdrawResult
  , ForeignWithdrawalHistoryItem
  , InitWithdrawal
  , InstitutionApi
  , Withdraw
  , WithdrawResult
  , WithdrawResultStatus(..)
  , WithdrawalHistory
  , WithdrawalHistoryItem
  , WithdrawalStatus
  , WithdrawalStatusWrapper
  , WithdrawalStatusesWrapper
  , _amountB
  , _amountW
  , _created
  , _currencyB
  , _currencyW
  , _initiator
  , _walletIdent
  , _withdrawalStatus
  , decodeWithdrawResultStatus
  , decodeWithdrawalStatus
  , fetchWithdrawHistoryPage
  , getWithdrawalFlowStatuses
  , getWithdrawalRegisterStatus
  , initWithdrawal
  , mkColour
  , mkInstitutionApi
  , withdraw
  )
  where

import Prelude

import BCorrespondent.Api.Foreign.Common
import BCorrespondent.Api.Foreign.Frontend (Currency, encodeCurrency)

import Data.Function.Uncurried (Fn1, Fn2, runFn2, Fn3,runFn3)
import Effect (Effect)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign)
import Data.Lens
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Foreign.Enum
import Data.Array (singleton, (:))

foreign import data InstitutionApi :: Type

foreign import mkInstitutionApi :: Fn1 ApiClient (Effect InstitutionApi)

type ForeignBalance = { currency :: Foreign, amount :: Number, walletIdent :: Int }

_currencyB = lens _.currency $ \el x -> el { currency = x }
_amountB = lens _.amount $ \el x -> el { amount = x }
_walletIdent = lens _.walletIdent $ \el x -> el { walletIdent = x }

type Balance = { currency :: Currency, amount :: Number, walletIdent :: Int }

type ForeignWithdrawalHistoryItem = 
     { ident :: Int,
       initiator :: String,
       ident :: Int, 
       currency :: Foreign, 
       amount :: Number, 
       withdrawalStatus :: Foreign, 
       created :: String 
     }

_currencyW = lens _.currency $ \el x -> el { currency = x }
_withdrawalStatus = lens _.withdrawalStatus $ \el x -> el { withdrawalStatus = x }
_ident = lens _.ident $ \el x -> el { ident = x }
_initiator = lens _.initiator $ \el x -> el { initiator = x }
_amountW = lens _.amount $ \el x -> el { amount = x }
_created = lens _.created $ \el x -> el { created = x }

data WithdrawalStatus = WithdrawalStatusNotResolved | Registered | Processing | Confirmed | Declined

derive instance genericWithdrawalStatus :: Generic WithdrawalStatus _
derive instance eqWithdrawalStatus :: Eq WithdrawalStatus

decodeWithdrawalStatus :: Foreign -> WithdrawalStatus
decodeWithdrawalStatus = fromMaybe WithdrawalStatusNotResolved <<< decodeEnumG

instance Show WithdrawalStatus where
  show WithdrawalStatusNotResolved = "-"
  show Registered = "registered"
  show Processing = "pending"
  show Confirmed = "ok"
  show Declined = "declined"

mkColour :: WithdrawalStatus -> String
mkColour WithdrawalStatusNotResolved = "black"
mkColour Registered = "black"
mkColour Processing = "black"
mkColour Confirmed  = "breen"
mkColour Declined  = "red"

type WithdrawalStatusWrapper = { status :: WithdrawalStatus }

getWithdrawalRegisterStatus :: WithdrawalStatusWrapper
getWithdrawalRegisterStatus = { status: Registered  }

type WithdrawalStatusesWrapper = { statuses :: Array WithdrawalStatus }

getWithdrawalFlowStatuses :: WithdrawalStatusesWrapper
getWithdrawalFlowStatuses = { statuses: Processing : Confirmed : singleton Declined }

type WithdrawalHistoryItem =
     { ident :: Int, 
       currency :: Currency, 
       initiator :: String,
       amount :: Number, 
       withdrawalStatus :: WithdrawalStatus, 
       created :: String
     }

type WithdrawalHistory = 
     { total :: Int,
       items :: Array ForeignWithdrawalHistoryItem
     }

type InitWithdrawal = 
     { walletBalances :: Array ForeignBalance, 
       history :: WithdrawalHistory
     }

foreign import _initWithdrawal :: Fn2 WithError InstitutionApi (AC.EffectFnAff (Object (Response InitWithdrawal)))

initWithdrawal :: InstitutionApi -> AC.EffectFnAff (Object (Response InitWithdrawal))
initWithdrawal = runFn2 _initWithdrawal withError

type Withdraw = { amount :: Number, walletIdent :: Int }

data WithdrawResultStatus = 
        WithdrawResultStatusNotResolved 
      | NotEnoughFunds 
      | WithdrawalRegistered
      | FrozenFunds

derive instance genericWithdrawResultStatus :: Generic WithdrawResultStatus _

decodeWithdrawResultStatus :: Foreign -> WithdrawResultStatus
decodeWithdrawResultStatus = fromMaybe WithdrawResultStatusNotResolved <<< decodeEnumG

type ForeignWithdrawResult = { frozenFunds :: Foreign, status :: Foreign }

type WithdrawResult = { frozenFunds :: Foreign, status :: WithdrawResultStatus }

foreign import _withdraw :: Fn3 WithError Withdraw InstitutionApi (AC.EffectFnAff (Object (Response ForeignWithdrawResult)))

withdraw :: Withdraw -> InstitutionApi -> AC.EffectFnAff (Object (Response ForeignWithdrawResult))
withdraw = runFn3 _withdraw withError

foreign import _fetchWithdrawHistoryPage :: Fn3 WithError Int InstitutionApi (AC.EffectFnAff (Object (Response WithdrawalHistory)))

fetchWithdrawHistoryPage :: Int -> InstitutionApi -> AC.EffectFnAff (Object (Response WithdrawalHistory))
fetchWithdrawHistoryPage = runFn3 _fetchWithdrawHistoryPage withError