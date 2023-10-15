module BCorrespondent.Api.Foreign.Frontend
  ( Currency(..)
  , DailyBalanceSheet
  , Direction(..)
  , EnumResolvedWallet
  , FrontApi
  , GapItem
  , GapItemAmount
  , GapItemTime
  , GapItemUnit
  , GapItemUnitStatus(..)
  , Init
  , InvoiceSince
  , NextGap
  , Sha
  , Transaction
  , TransactionValue
  , Wallet
  , WalletType(..)
  , _amount
  , _amounts
  , _correspondentBank
  , _correspondentBankSwiftSepaCode
  , _currency
  , _elements
  , _end
  , _hour
  , _ident
  , _min
  , _senderAddress
  , _senderBank
  , _senderBankAccount
  , _senderName
  , _senderPhoneNumber
  , _start
  , _swiftSepaCode
  , _transaction
  , decodeCurrency
  , decodeGapItemUnitStatus
  , decodeWalletType
  , fetchTimelineForParticularHour
  , fetchTrnsaction
  , getJwtStatus
  , init
  , initDashboard
  , loadNextGap
  , mkFrontApi
  , printGapItemAmount
  , printGapItemUnit
  , printInit
  , shaPred
  )
  where

import Prelude

import BCorrespondent.Api.Foreign.Common

import Data.Function.Uncurried (Fn1, Fn3, runFn3, Fn2, runFn2, runFn4, Fn4)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Maybe (Maybe(..), fromMaybe)
import Undefined
import Foreign (Foreign)
import Effect (Effect)
import Data.Map as Map
import Data.Either (Either)
import Effect.Exception as E
import Data.Array (uncons)
import Data.Lens (lens, Lens)
import Data.Generic.Rep (class Generic)
import Foreign.Enum
import Data.String (toLower)
import Data.Either (hush)
import Control.Monad.Except (runExcept)

import Undefined

foreign import data FrontApi :: Type

foreign import mkFrontApi :: Fn1 ApiClient (Effect FrontApi)

decodeEnumG :: forall a rep . Generic a rep => GenericDecodeEnum rep => Foreign -> Maybe a
decodeEnumG = hush <<< runExcept <<< genericDecodeEnum {constructorTagTransform: toLower}

type Sha = { key :: String, value :: String }

printSha { key, value } = "{ \"key\": " <> key <> ", \"value\": " <> value <> " }"

shaPred :: String -> Sha -> Boolean
shaPred s {key} = s == key

type InvoiceSince = { year :: Int, month :: Int, day :: Int }

printInvoiceSince {year, month, day} = 
  "{year:" <> show year  <> ",month:" <> show month <> ", day:" <> show day <> "}"

type Init = 
     { isjwtvalid :: String, 
       shaxs :: Array Sha, 
       level :: String, 
       totelegram :: Boolean,
       telegramchat :: String,
       telegrambot :: String,
       loadcsslocally :: Boolean,
       invoicesince :: InvoiceSince
     }

printShaxs xs = 
  case uncons xs of 
    Just { head, tail } -> 
      printSha head <> printShaxs tail
    Nothing -> mempty

printInit 
  {isjwtvalid, 
   shaxs, 
   level, 
   totelegram, 
   telegramchat, 
   telegrambot, 
   loadcsslocally,
   invoicesince
  } = 
  "{ \"isjwtvalid\": " <> isjwtvalid <> 
  ", \"shaxs\": [ " <> printShaxs shaxs <> "] " <>
  ", \"level\": " <> level <>
  ", \"totelegram\": " <> show totelegram <>
  ", \"telegramchat\": " <> telegramchat <>
  ", \"telegrambot\": " <> telegrambot <>
  ", \"loadCssLocally\": " <> show loadcsslocally <> 
  ", \"invoicesince\": " <> printInvoiceSince invoicesince <> " }"

getJwtStatus :: String -> Maybe JWTStatus
getJwtStatus "valid" = Just Valid
getJwtStatus "invalid" = Just Invalid 
getJwtStatus "skip" = Just Skip
getJwtStatus _ = Nothing

foreign import _init :: Fn3 WithError Json FrontApi (AC.EffectFnAff (Object (Response Init)))

init :: Maybe JWTToken -> FrontApi -> AC.EffectFnAff (Object (Response Init))
init token = runFn3 _init withError (fromMaybe jsonEmptyObject (map encodeJson token))

data GapItemUnitStatus = Pending | Ok | Declined

derive instance genericGapItemUnitStatus :: Generic GapItemUnitStatus _
derive instance eqGapItemUnitStatus :: Eq GapItemUnitStatus

instance Show GapItemUnitStatus where
  show Pending = "pending"
  show Ok = "ok"
  show Declined = "declined"

decodeGapItemUnitStatus :: Foreign -> Maybe GapItemUnitStatus
decodeGapItemUnitStatus = decodeEnumG

type GapItemUnit = 
     { status :: Foreign, 
       textualIdent :: String, 
       ident :: Int, 
       tm :: String
     }

printGapItemUnit {status, textualIdent, ident, tm} = 
  "{status: foreign, ident: " <> 
  show ident <> 
  ", textualIdent: " <> textualIdent <> 
  ", tm: " <> tm  <> "}"

type GapItemTime = { hour :: Int, min :: Int }

_hour = lens _.hour $ \el x -> el { hour = x }
_min = lens _.min $ \el x -> el { min = x }

type GapItemAmount = { currency :: Foreign, value :: Number }

printGapItemAmount { currency, value } = "{currency: " <> show (decodeCurrency currency) <> ", value:" <> show value <> "}"

type GapItem = 
     { elements :: Array GapItemUnit, 
       start :: GapItemTime, 
       end :: GapItemTime,
       amounts :: Array GapItemAmount
     }

_start = lens _.start $ \el x -> el { start = x }
_end = lens _.end $ \el x -> el { end = x }
_elements = lens _.elements $ \el x -> el { elements = x }
_amounts = lens _.amounts $ \el x -> el { amounts = x }

type DailyBalanceSheet = { institution :: String, gaps :: Array GapItem }

data WalletType = Debit | Credit | WalletTypeNotResolved

derive instance genericWalletType :: Generic WalletType _
derive instance eqWalletType :: Eq WalletType
derive instance ordWalletType :: Ord WalletType

instance Show WalletType where
  show Debit = "debit"
  show Credit = "credit"
  show WalletTypeNotResolved = "wallet type not resolved"

decodeWalletType :: Foreign -> Maybe WalletType
decodeWalletType = decodeEnumG

data Currency = USD | EUR | CurrencyNotResolved

derive instance genericCurrency :: Generic Currency _
derive instance eqCurrency :: Eq Currency
derive instance ordCurrency :: Ord Currency

instance Show Currency where
  show USD = "usd"
  show EUR = "eur"
  show CurrencyNotResolved = "currency type not resolved"

decodeCurrency :: Foreign -> Maybe Currency
decodeCurrency = decodeEnumG

type Wallet = 
    { ident :: Int,
      walletType :: Foreign,
      currency :: Foreign, 
      amount :: Number 
    }

type EnumResolvedWallet = 
    { ident :: Int,
      walletType :: WalletType,
      currency :: Currency, 
      amount :: Number 
    }

type Dashboard = 
     { dailyBalanceSheet :: DailyBalanceSheet, 
       wallets :: Array Wallet 
     }

foreign import _initDashboard :: Fn2 WithError FrontApi (AC.EffectFnAff (Object (Response Dashboard)))

initDashboard :: FrontApi -> AC.EffectFnAff (Object (Response Dashboard))
initDashboard = runFn2 _initDashboard withError

foreign import _loadNextGap :: Fn4 WithError String String FrontApi (AC.EffectFnAff (Object (Response NextGap)))

type NextGap = { gap :: GapItem }

loadNextGap :: String -> String -> FrontApi -> AC.EffectFnAff (Object (Response NextGap))
loadNextGap = runFn4 _loadNextGap withError

foreign import _fetchTimelineForParticularHour :: Fn4 WithError Foreign String FrontApi (AC.EffectFnAff (Object (Response (Array GapItem))))

data Direction = Backward | Forward

derive instance genericDirection :: Generic Direction _

encodeDirection :: Direction -> Foreign
encodeDirection = genericEncodeEnum {constructorTagTransform: toLower}

fetchTimelineForParticularHour :: Direction -> String -> FrontApi -> AC.EffectFnAff (Object (Response (Array GapItem)))
fetchTimelineForParticularHour direction = runFn4 _fetchTimelineForParticularHour withError (encodeDirection direction)

type Transaction = { transaction :: TransactionValue }

_transaction = lens _.transaction $ \el x -> el { transaction = x }

type TransactionValue = 
     { correspondentBank :: String,
       correspondentBankSwiftSepaCode :: String,
       currency :: String,
       ident :: String,
       senderAddress :: String,
       senderBank :: String,
       senderBankAccount :: String,
       senderName :: String,
       senderPhoneNumber :: String,
       swiftSepaCode :: String,
       amount :: Number
     }

_correspondentBank = lens _.correspondentBank $ \el x -> el { correspondentBank = x }
_correspondentBankSwiftSepaCode = lens _.correspondentBankSwiftSepaCode $ \el x -> el { correspondentBankSwiftSepaCode = x }
_currency = lens _.currency $ \el x -> el { currency = x }
_ident = lens _.ident $ \el x -> el { ident = x }
_senderAddress = lens _.senderAddress $ \el x -> el { senderAddress = x }
_senderBank = lens _.senderBank $ \el x -> el { senderBank = x }
_senderBankAccount = lens _.senderBankAccount $ \el x -> el { senderBankAccount = x }
_senderName = lens _.senderName $ \el x -> el { senderName = x }
_senderPhoneNumber = lens _.senderPhoneNumber $ \el x -> el { senderPhoneNumber = x }
_swiftSepaCode = lens _.swiftSepaCode $ \el x -> el { swiftSepaCode = x }
_amount = lens _.amount $ \el x -> el { amount = x }

foreign import _fetchTransaction :: Fn3 WithError Int FrontApi (AC.EffectFnAff (Object (Response Transaction)))

fetchTrnsaction :: Int -> FrontApi -> AC.EffectFnAff (Object (Response Transaction))
fetchTrnsaction = runFn3 _fetchTransaction withError


