module BCorrespondent.Api.Foreign.Frontend
  ( AmountInDayOfWeek
  , BalancedBook
  , BalancedBookInstitution
  , Balances
  , Currency(..)
  , DailyBalanceSheet
  , DayOfWeekHourly
  , Direction(..)
  , EnumResolvedWallet
  , Fee(..)
  , FetchShiftHistoryTimelineParams
  , ForeignDayOfWeeksHourlyTotalSum
  , ForeignTransaction
  , ForeignTransactionOk
  , ForeignTransactionOption
  , FromNotification
  , FrontApi
  , GapItem
  , GapItemAmount
  , GapItemTime
  , GapItemUnit
  , GapItemUnitStatus(..)
  , GapItemWrapper
  , HistoryTimeline
  , Init
  , InvoiceSince
  , Issue
  , NextGap
  , NoForeignTransaction
  , Notification
  , Notifications
  , Sha
  , Transaction
  , TransactionFailure
  , TransactionOk
  , Wallet
  , WalletType(..)
  , Workspace
  , _amount
  , _amounts
  , _balancesBalancedBook
  , _charges
  , _currency
  , _description
  , _elements
  , _end
  , _hour
  , _ident
  , _institutionBalancedBook
  , _min
  , _reason
  , _receiver
  , _receiverBank
  , _sender
  , _senderBank
  , _senderCity
  , _senderCountry
  , _start
  , _timestamp
  , _transaction
  , decodeCurrency
  , decodeFee
  , decodeGapItemUnitStatus
  , decodeWalletType
  , encodeCurrency
  , encodeDirection
  , encodeFee
  , fetchBalancedBook
  , fetchNotifications
  , fetchShiftHistoryTimeline
  , fetchTimelineForParticularHour
  , fetchTrnsaction
  , getJwtStatus
  , init
  , initBalancedBook
  , initDashboard
  , initHistoryTimeline
  , loadNextGap
  , loadUnreadNotification
  , markNotificationRead
  , mkFrontApi
  , printGapItemAmount
  , printGapItemUnit
  , printInit
  , printTimline
  , resolveTransaction
  , shaPred
  , submitIssue
  )
  where

import Prelude

import BCorrespondent.Api.Foreign.Common

import Data.Function.Uncurried (Fn1, Fn3, runFn3, Fn2, runFn2, runFn4, Fn4, Fn6, runFn6)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Maybe (Maybe(..), fromMaybe)
import Undefined
import Foreign (Foreign, isUndefined, tagOf, unsafeFromForeign)
import Effect (Effect)
import Data.Map as Map
import Data.Either (Either (..))
import Effect.Exception as E
import Data.Array (uncons)
import Data.Lens (lens, Lens, (%~), (^.))
import Data.Generic.Rep (class Generic)
import Foreign.Enum
import Data.String (toLower)
import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericToEnum, genericFromEnum, genericSucc, genericPred)
import Effect.Exception (Error, error)


import Undefined

foreign import data FrontApi :: Type

foreign import mkFrontApi :: Fn1 ApiClient (Effect FrontApi)

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

data GapItemUnitStatus = Pending | Ok | Declined | GapItemUnitStatusNotResolved

derive instance genericGapItemUnitStatus :: Generic GapItemUnitStatus _
derive instance eqGapItemUnitStatus :: Eq GapItemUnitStatus

instance Show GapItemUnitStatus where
  show GapItemUnitStatusNotResolved = "unknown"
  show Pending = "pending"
  show Ok = "ok"
  show Declined = "declined"

decodeGapItemUnitStatus :: Foreign -> GapItemUnitStatus
decodeGapItemUnitStatus = fromMaybe GapItemUnitStatusNotResolved <<< decodeEnumG

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

decodeWalletType :: Foreign -> WalletType
decodeWalletType = fromMaybe WalletTypeNotResolved <<< decodeEnumG

data Currency = CurrencyNotResolved | USD | EUR

derive instance genericCurrency :: Generic Currency _
derive instance eqCurrency :: Eq Currency
derive instance ordCurrency :: Ord Currency

instance Enum Currency where
  succ = genericSucc
  pred = genericPred

instance Bounded Currency where
  top = USD
  bottom = CurrencyNotResolved

instance BoundedEnum Currency where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show Currency where
  show USD = "USD"
  show EUR = "EUR"
  show CurrencyNotResolved = "currency type not resolved"

decodeCurrency :: Foreign -> Currency
decodeCurrency = fromMaybe CurrencyNotResolved <<< decodeEnumG

encodeCurrency :: Currency -> Foreign
encodeCurrency = genericEncodeEnum defaultGenericEnumOptions { constructorTagTransform = toLower }

data Fee = FeeNotResolved | OUR | SHA

derive instance genericFee :: Generic Fee _
derive instance eqFee :: Eq Fee
derive instance ordFee :: Ord Fee

instance Enum Fee where
  succ = genericSucc
  pred = genericPred

instance Bounded Fee where
  top = SHA
  bottom = FeeNotResolved

instance BoundedEnum Fee where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show Fee where
  show OUR = "OUR"
  show SHA = "SHA"
  show FeeNotResolved = "fee type not resolved"

decodeFee :: Foreign -> Fee
decodeFee = fromMaybe FeeNotResolved <<< decodeEnumG

encodeFee :: Fee -> Foreign
encodeFee = genericEncodeEnum defaultGenericEnumOptions { constructorTagTransform = toLower }


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

type GapItemWrapper = { items :: Array GapItem }

foreign import _fetchTimelineForParticularHour :: Fn4 WithError Foreign String FrontApi (AC.EffectFnAff (Object (Response GapItemWrapper)))

data Direction = Backward | Forward

instance Show Direction where
  show Backward = "backward"
  show Forward = "forward"

derive instance genericDirection :: Generic Direction _
derive instance eqDirection :: Eq Direction

encodeDirection :: Direction -> Foreign
encodeDirection = genericEncodeEnum {constructorTagTransform: toLower}

fetchTimelineForParticularHour :: Direction -> String -> FrontApi -> AC.EffectFnAff (Object (Response GapItemWrapper))
fetchTimelineForParticularHour direction = runFn4 _fetchTimelineForParticularHour withError (encodeDirection direction)

type ForeignTransaction = { transaction :: ForeignTransactionOption }

type ForeignTransactionOption = { ok :: Foreign, failure :: Foreign }

type ForeignTransactionOk = 
     { sender :: String,
       senderCountry :: String,
       senderCity :: String,
       senderBank :: String,
      --  receiver :: String,
       receiverBank :: String,
       correspondentBank :: String,
       amount :: Number,
       currency :: Foreign,
       description :: String,
       charges :: Foreign,
       tm :: String
     }

type TransactionOk = 
     { sender :: String,
       senderCountry :: String,
       senderCity :: String,
       senderBank :: String,
      --  receiver :: String,
       receiverBank :: String,
       correspondentBank :: String,
       amount :: Number,
       currency :: Currency,
       description :: String,
       charges :: Fee,
       tm :: String
     }

type TransactionFailure = { reason :: String, tm :: String }

type Transaction = { ok :: Maybe ForeignTransactionOk, failure :: Maybe TransactionFailure }

type NoForeignTransaction = { ok :: Maybe TransactionOk, failure :: Maybe TransactionFailure }

resolveTransaction :: ForeignTransaction -> Either Error NoForeignTransaction
resolveTransaction { transaction: { ok: okValue, failure: failureValue } } 
  | isUndefined failureValue && 
     tagOf okValue  == "Object"
     = let fok = unsafeFromForeign okValue :: ForeignTransactionOk
           rok = fok # _currency %~ decodeCurrency 
                    # _charges %~ decodeFee
       in Right { ok: Just rok, failure: Nothing }
  | isUndefined okValue && 
    tagOf failureValue  == "Object"
     = Right { failure: Just (unsafeFromForeign failureValue), ok: Nothing }
  | otherwise = Left $ error "cannot resolve transaction"

_transaction = lens _.transaction $ \el x -> el { transaction = x }
_sender = lens _.sender $ \el x -> el { sender = x }
_senderCountry = lens _.senderCountry $ \el x -> el { senderCountry = x }
_senderCity = lens _.senderCity $ \el x -> el { senderCity = x }
_senderBank = lens _.senderBank $ \el x -> el { senderBank = x }
_receiver = lens _.receiver $ \el x -> el { receiver = x }
_receiverBank = lens _.receiverBank $ \el x -> el { receiverBank = x }
_amount = lens _.amount $ \el x -> el { amount = x }
_currency = lens _.currency $ \el x -> el { currency = x }
_description = lens _.description $ \el x -> el { description = x }
_charges = lens _.charges $ \el x -> el { charges = x }
_timestamp = lens _.tm $ \el x -> el { tm = x }
_reason = lens _.reason $ \el x -> el { reason = x }

foreign import _fetchTransaction :: Fn3 WithError Int FrontApi (AC.EffectFnAff (Object (Response ForeignTransaction)))

fetchTrnsaction :: Int -> FrontApi -> AC.EffectFnAff (Object (Response ForeignTransaction))
fetchTrnsaction = runFn3 _fetchTransaction withError

type HistoryTimeline = 
     { institution :: String, 
       timeline :: Array GapItem 
     }

type Date = { year :: Int, month :: Int, day :: Int }

printDate {year, month, day} = show year <> "," <> show month <> "," <> show day 

foreign import _initHistoryTimeline :: Fn3 WithError String FrontApi (AC.EffectFnAff (Object (Response HistoryTimeline)))

initHistoryTimeline :: Date -> FrontApi -> AC.EffectFnAff (Object (Response HistoryTimeline))
initHistoryTimeline date = runFn3 _initHistoryTimeline withError (printDate date)

type FetchShiftHistoryTimelineParams =
     { year :: Int,
       month :: Int,
       day :: Int,
       direction :: Foreign,
       hour :: Int,
       institution :: Int
     }

foreign import _fetchShiftHistoryTimeline :: Fn3 WithError FetchShiftHistoryTimelineParams FrontApi (AC.EffectFnAff (Object (Response GapItemWrapper)))

fetchShiftHistoryTimeline :: FetchShiftHistoryTimelineParams -> FrontApi -> AC.EffectFnAff (Object (Response GapItemWrapper))
fetchShiftHistoryTimeline = runFn3 _fetchShiftHistoryTimeline withError

printTimline timeline = 
  flip map (timeline :: Array GapItem) \ x -> 
    x # _elements %~ map printGapItemUnit 
      # _amounts %~ map printGapItemAmount

type Notification = { ident :: Int, text :: String } 

type Notifications = { count :: Int, items :: Array Notification }

foreign import _fetchNotifications :: Fn3 WithError FromNotification FrontApi (AC.EffectFnAff (Object (Response Notifications)))

type FromNotification = { from :: Int }

fetchNotifications :: FromNotification -> FrontApi -> AC.EffectFnAff (Object (Response Notifications))
fetchNotifications = runFn3 _fetchNotifications withError

type Issue = { description :: String, files :: Array Int }

foreign import _submitIssue  :: Fn3 WithError Issue FrontApi (AC.EffectFnAff (Object (Response Unit)))

submitIssue :: Issue -> FrontApi -> AC.EffectFnAff (Object (Response Unit))
submitIssue = runFn3 _submitIssue withError

foreign import _markNotificationRead :: Fn3 WithError (Array Int) FrontApi (AC.EffectFnAff (Object (Response Unit)))

markNotificationRead :: Array Int -> FrontApi -> AC.EffectFnAff (Object (Response Unit))
markNotificationRead = runFn3 _markNotificationRead withError

type AmountInDayOfWeek = { value :: Int, total :: Int }

type ForeignDayOfWeeksHourlyTotalSum = 
     { amount :: Number, 
       currency :: Foreign  
     }

type DayOfWeekHourly = 
     { from :: GapItemTime, 
       to :: GapItemTime, 
       total :: Array ForeignDayOfWeeksHourlyTotalSum,
       amountInDayOfWeek :: Array AmountInDayOfWeek 
     }

type Balances = 
     { ident :: Int,
       amount :: Number,
       currency :: Foreign,
       walletType :: Foreign
     }

type BalancedBookInstitution = 
     { title :: String,
       ident :: Int,
       dayOfWeeksHourly :: Array DayOfWeekHourly,
       balances :: Array Balances
     }

type BalancedBook =
     { from :: String,
       to :: String,
       institutions :: Array BalancedBookInstitution 
     }

_institutionBalancedBook = lens _.institutions $ \el x -> el { institutions = x }
_balancesBalancedBook = lens _.balances $ \el x -> el { balances = x }


foreign import _initBalancedBook :: Fn2 WithError FrontApi (AC.EffectFnAff (Object (Response BalancedBook)))

initBalancedBook :: FrontApi -> AC.EffectFnAff (Object (Response BalancedBook))
initBalancedBook = runFn2 _initBalancedBook withError

foreign import _fetchBalancedBook :: Fn6 WithError Int Int Int Foreign FrontApi (AC.EffectFnAff (Object (Response BalancedBook)))

fetchBalancedBook :: Int -> Int -> Int -> Direction -> FrontApi -> AC.EffectFnAff (Object (Response BalancedBook))
fetchBalancedBook y m d direction = runFn6 _fetchBalancedBook withError y m d (encodeDirection direction)

type Workspace = { unreadNotification :: Int }

foreign import _loadUnreadNotification :: Fn2 WithError FrontApi (AC.EffectFnAff (Object (Response Workspace)))

loadUnreadNotification :: FrontApi -> AC.EffectFnAff (Object (Response Workspace))
loadUnreadNotification = runFn2 _loadUnreadNotification withError

_ident = lens _.ident $ \el x -> el { ident = x }