module BCorrespondent.Api.Foreign.Frontend
  ( AmountInDayOfWeek
  , BalancedBook
  , BalancedBookInstitution
  , Currency(..)
  , DailyBalanceSheet
  , DayOfWeekHourly
  , Direction(..)
  , EnumResolvedWallet
  , FetchShiftHistoryTimelineParams
  , ForeignDayOfWeeksHourlyTotalSum
  , FrontApi
  , GapItem
  , GapItemAmount
  , GapItemTime
  , GapItemUnit
  , GapItemUnitStatus(..)
  , HistoryTimeline
  , Init
  , InvoiceSince
  , Issue
  , NextGap
  , Notification
  , Notifications
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
  , encodeCurrency
  , encodeDirection
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
  , markNotificationRead
  , mkFrontApi
  , printGapItemAmount
  , printGapItemUnit
  , printInit
  , printTimline
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
import Foreign (Foreign)
import Effect (Effect)
import Data.Map as Map
import Data.Either (Either)
import Effect.Exception as E
import Data.Array (uncons)
import Data.Lens (lens, Lens, (%~))
import Data.Generic.Rep (class Generic)
import Foreign.Enum
import Data.String (toLower)
import Data.Enum (class Enum, class BoundedEnum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericToEnum, genericFromEnum, genericSucc, genericPred)

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

instance Show Direction where
  show Backward = "backward"
  show Forward = "forward"

derive instance genericDirection :: Generic Direction _
derive instance eqDirection :: Eq Direction

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
       hour :: Int 
     }

foreign import _fetchShiftHistoryTimeline :: Fn3 WithError FetchShiftHistoryTimelineParams FrontApi (AC.EffectFnAff (Object (Response (Array GapItem))))

fetchShiftHistoryTimeline :: FetchShiftHistoryTimelineParams -> FrontApi -> AC.EffectFnAff (Object (Response (Array GapItem)))
fetchShiftHistoryTimeline = runFn3 _fetchShiftHistoryTimeline withError

printTimline timeline = 
  flip map (timeline :: Array GapItem) \ x -> 
    x # _elements %~ map printGapItemUnit 
      # _amounts %~ map printGapItemAmount

type Notification = { ident :: Int, text :: String } 

type Notifications = { count :: Int, items :: Array Notification }

foreign import _fetchNotifications :: Fn2 WithError FrontApi (AC.EffectFnAff (Object (Response Notifications)))

fetchNotifications :: FrontApi -> AC.EffectFnAff (Object (Response Notifications))
fetchNotifications = runFn2 _fetchNotifications withError

type Issue = { description :: String, files :: Array Int }

foreign import _submitIssue  :: Fn3 WithError Issue FrontApi (AC.EffectFnAff (Object (Response Unit)))

submitIssue :: Issue -> FrontApi -> AC.EffectFnAff (Object (Response Unit))
submitIssue = runFn3 _submitIssue withError

foreign import _markNotificationRead :: Fn3 WithError Int FrontApi (AC.EffectFnAff (Object (Response Unit)))

markNotificationRead :: Int -> FrontApi -> AC.EffectFnAff (Object (Response Unit))
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

type BalancedBookInstitution = 
     { title :: String,
       dayOfWeeksHourly :: Array DayOfWeekHourly
     }

type BalancedBook =
     { from :: String,
       to :: String,
       institutions :: Array BalancedBookInstitution 
     }

foreign import _initBalancedBook :: Fn2 WithError FrontApi (AC.EffectFnAff (Object (Response BalancedBook)))

initBalancedBook :: FrontApi -> AC.EffectFnAff (Object (Response BalancedBook))
initBalancedBook = runFn2 _initBalancedBook withError

foreign import _fetchBalancedBook :: Fn6 WithError Int Int Int Foreign FrontApi (AC.EffectFnAff (Object (Response BalancedBook)))

fetchBalancedBook :: Int -> Int -> Int -> Direction -> FrontApi -> AC.EffectFnAff (Object (Response BalancedBook))
fetchBalancedBook y m d direction = runFn6 _fetchBalancedBook withError y m d (encodeDirection direction)
