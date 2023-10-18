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
  , _amountB
  , _currencyB
  , _walletIdent
  , decodeWithdrawResultStatus
  , initWithdrawal
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

foreign import data InstitutionApi :: Type

foreign import mkInstitutionApi :: Fn1 ApiClient (Effect InstitutionApi)

type ForeignBalance = { currency :: Foreign, amount :: Number, walletIdent :: Int }

_currencyB = lens _.currency $ \el x -> el { currency = x }
_amountB = lens _.amount $ \el x -> el { amount = x }
_walletIdent = lens _.walletIdent $ \el x -> el { walletIdent = x }

type Balance = { currency :: Currency, amount :: Number, walletIdent :: Int }

type ForeignWithdrawalHistoryItem = 
     { ident :: Int, 
       currency :: Foreign, 
       amount :: Number, 
       withdrawalStatus :: Foreign, 
       created :: String 
     }

type InitWithdrawal = 
     { walletBalances :: Array ForeignBalance, 
       history :: Array ForeignWithdrawalHistoryItem 
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

derive instance genericDirection :: Generic WithdrawResultStatus _

decodeWithdrawResultStatus :: Foreign -> WithdrawResultStatus
decodeWithdrawResultStatus = fromMaybe WithdrawResultStatusNotResolved <<< decodeEnumG

type ForeignWithdrawResult = { frozenFunds :: Foreign, status :: Foreign }

type WithdrawResult = { frozenFunds :: Foreign, status :: WithdrawResultStatus }

foreign import _withdraw :: Fn3 WithError Withdraw InstitutionApi (AC.EffectFnAff (Object (Response ForeignWithdrawResult)))

withdraw :: Withdraw -> InstitutionApi -> AC.EffectFnAff (Object (Response ForeignWithdrawResult))
withdraw = runFn3 _withdraw withError
