module BCorrespondent.Api.Foreign.Institution
  ( Balance
  , Balances
  , InstitutionApi
  , Withdraw
  , fetchBalances
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

foreign import data InstitutionApi :: Type

foreign import mkInstitutionApi :: Fn1 ApiClient (Effect InstitutionApi)

type Balance = { currency :: Currency, amount :: Number }

type Balances = { xs :: Array Balance }

foreign import _fetchBalances :: Fn2 WithError InstitutionApi (AC.EffectFnAff (Object (Response Balances)))

fetchBalances :: InstitutionApi -> AC.EffectFnAff (Object (Response Balances))
fetchBalances = runFn2 _fetchBalances withError

type Withdraw = { amount :: Number, currency :: Currency }

type ForeignWithdraw = { amount :: Number, currency :: Foreign }

foreign import _withdraw :: Fn3 WithError ForeignWithdraw InstitutionApi (AC.EffectFnAff (Object (Response Unit)))

withdraw :: Withdraw -> InstitutionApi -> AC.EffectFnAff (Object (Response Unit))
withdraw {amount: sum, currency: old} = runFn3 _withdraw withError ({ amount: sum, currency: encodeCurrency old })
