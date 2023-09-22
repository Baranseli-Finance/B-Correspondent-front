module Buzgibi.Component.List
  ( Action(..)
  , Report
  , Voice
  , component
  , proxy
  ) where

import Prelude

import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Data.Config
import Buzgibi.Component.HTML.Utils (css, safeHref, maybeElem)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Component.Async (withAffjax)
import Buzgibi.Component.Pagination as Pagination
import Buzgibi.Component.Subscription.Pagination as Pagination
import Buzgibi.Component.Async as Async
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Data.Route (Route(UserSurvey))
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route
import Buzgibi.Component.Subscription.WS (subscribe) as WS

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe(..))
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Affjax.Web as AX
import Affjax.ResponseFormat as AX
import Affjax.RequestBody as AXB
import File.Blob (downloadBlob)
import Data.Array (snoc)
import Data.String (length, take, joinWith)
import Effect.AVar as Async
import DOM.HTML.Indexed.ScopeValue (ScopeValue(ScopeCol))
import Foreign (isUndefined, unsafeFromForeign)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Undefined
import Data.Array ((:))
import DOM.HTML.Indexed.ButtonType (ButtonType(ButtonButton))
import Data.Foldable (for_)
import Data.Traversable (for)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Foreign (Foreign, unsafeToForeign, unsafeFromForeign, isNull)
import Effect.AVar as Async
import Web.Socket as WS
import Effect.Aff as Aff
import Data.Array (uncons)
import Date.Format (format)
import Data.Function.Uncurried (runFn1)

proxy = Proxy :: _ "list"

loc = "Buzgibi.Component.List"

type State =
  { list :: Array BuzgibiBack.WithFieldStatusHistoryItem
  , total :: Int
  , perpage :: Int
  , hash :: String
  , constants :: Map.Map String String
  , currPage :: Int
  }

type Voice = { survey :: Int, voice :: Int }

type Report = { survey :: Int, report :: Int, status :: String }

data Action
  = Initialize
  | Download Int String Event
  | Query Int
  | LangChange String (Map.Map String String)
  | Submit Int Boolean MouseEvent
  | Edit Int Foreign MouseEvent
  | CatchVoiceWS Voice
  | CatchReportWS Report
  | Finalize
  | TechnicalHitch Int MouseEvent

component =
  H.mkComponent
    { initialState: \{ page } ->
        { list: []
        , total: 0
        , perpage: 0
        , hash: mempty :: String
        , constants:
            Map.empty
        , currPage: page
        }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , finalize = pure Finalize
        }
    }
  where
  getHistory page = do
    { user, config: Config { apiBuzgibiHost } } <- getStore
    case user of
      Just { token } -> do
        resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ BuzgibiBack.getHistory page
        logDebug $ loc <> " ---> get history for page " <> show page
        withError resp \{ success: { items: old, total, perpage } } -> do
          logDebug $ loc <> " ---> get history items " <> joinWith "," (map BuzgibiBack.printWithFieldStatusHistoryItem old) <> " for page " <> show page
          new <- for old \item@{ timestamp } -> do
            tm <- H.liftEffect $ format timestamp
            pure item { timestamp = tm }
          H.modify_ _ { list = new, total = total, perpage = perpage, currPage = maybe 1 _.page page }
      Nothing -> pure unit
  handleAction Initialize = do
    void $ initTranslation loc \hash translation -> do
      let
        constants =
          fromMaybe undefined
            $ Map.lookup "history"
            $
              BuzgibiBack.getTranslationEndpoints translation
      logDebug $ loc <> " ---> conttants " <> show constants
      H.modify_ _ { hash = hash, constants = constants }
    { currPage } <- H.get
    getHistory $ Just { page: currPage }
    Pagination.subscribe loc $ handleAction <<< Query
    Translation.subscribe loc $ \hash translation ->
      let
        constants =
          fromMaybe undefined
            $ Map.lookup "history"
            $
              BuzgibiBack.getTranslationEndpoints translation
      in
        handleAction $ LangChange hash constants

    WS.subscribe loc "ws/user/survey/history/voice" (Just 1) $
      \{ success: val } -> handleAction $ CatchVoiceWS val
    WS.subscribe loc "ws/user/survey/history/report" (Just 1) $
      \{ success: o } ->
        if isNull o then
          pure unit
        else
          handleAction
            $ CatchReportWS
            $
              unsafeFromForeign o

  handleAction (Download ident name ev) = do
    H.liftEffect $ preventDefault ev
    logDebug $ loc <> " ---> downloaded file " <> name
    { config: Config { apiBuzgibiHost }, async, user } <- getStore
    case user of
      Just { jwtUser: { ident: userId } } ->
        H.liftAff $ do
          resp <- AX.get AX.blob $ apiBuzgibiHost <> "/file/download/" <> show userId <> "/raw/" <> show ident
          withAffjax loc async resp $ pure <<< flip downloadBlob name
      Nothing -> pure unit
  handleAction (Query page) = getHistory $ Just { page: page }
  handleAction (LangChange hash xs) = H.modify_ _ { constants = xs, hash = hash }
  handleAction (Submit _ true ev) = do
    H.liftEffect $ preventDefault $ toEvent ev
    { constants } <- H.get
    let value = fromMaybe "..." $ Map.lookup "bark" constants
    Async.send $ Async.mkOrdinary value Async.Warning Nothing
  handleAction (Submit ident false ev) = do
    H.liftEffect $ preventDefault $ toEvent ev
    { config: Config { apiBuzgibiHost }, user } <- getStore
    for_ user \{ token } -> do
      resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ BuzgibiBack.submitSurvey { ident: ident }
      withError resp \{ success: ifOk } -> do
        { constants } <- H.get
        if ifOk then do
          H.modify_ \s ->
            s
              { list =
                  _.list s <#> \el@{ surveyident } ->
                    if surveyident == ident then
                      el { status = "inProcess" }
                    else el
              }
          let msg = fromMaybe "..." $ Map.lookup "submitOk" constants
          Async.send $ Async.mkOrdinary msg Async.Success Nothing
        else do
          let msg = fromMaybe "..." $ Map.lookup "submitError" constants
          Async.send $ Async.mkOrdinary msg Async.Warning Nothing
  handleAction (Edit _ voice ev) | isUndefined voice = do
    H.liftEffect $ preventDefault $ toEvent ev
    { constants } <- H.get
    let value = fromMaybe "..." $ Map.lookup "bark" constants
    Async.send $ Async.mkOrdinary value Async.Warning Nothing
  handleAction (Edit ident voice ev) = do
    H.liftEffect $ preventDefault $ toEvent ev
    logDebug $ loc <> " ---> edit survey " <> show ident
    { editSurvey } <- getStore
    let unwrappedVoice = unsafeFromForeign voice
    void $ H.liftEffect $ { survey: ident, voice: unwrappedVoice } `Async.tryPut` editSurvey
    navigate $ Route.EditSurvey ident

  handleAction Finalize = do
    { wsVar } <- getStore
    wsm <- H.liftEffect $ Async.tryTake wsVar
    for_ wsm \xs ->
      for_ xs \{ ws, forkId } -> do
        H.kill forkId
        H.liftEffect $ WS.close ws
        logDebug $ loc <> " ---> ws has been killed"

  handleAction (CatchVoiceWS { survey, voice: voice_ident }) = do
    let
      insertVoice [] = []
      insertVoice array =
        case uncons array of
          Just { head: el@{ surveyident }, tail } ->
            if surveyident == survey then el { voice = unsafeToForeign voice_ident } : tail
            else el : insertVoice tail
          Nothing -> []
    H.modify_ \s -> s { list = insertVoice (_.list s) }

  handleAction (CatchReportWS { survey, report: report_ident, status }) = do
    logDebug $ loc <> " ---> ws report caught: " <> show survey
    let
      insertReport [] = []
      insertReport array =
        case uncons array of
          Just { head: el@{ surveyident }, tail } ->
            if surveyident == survey then el { reportident = unsafeToForeign report_ident, status = status } : tail
            else el : insertReport tail
          Nothing -> []
    H.modify_ \s -> s { list = insertReport (_.list s) }

  handleAction (TechnicalHitch ident ev) = do
    H.liftEffect $ preventDefault $ toEvent ev
    { config: Config { apiBuzgibiHost } } <- getStore
    email <- H.liftEffect $ runFn1 BuzgibiBack.mkSendGridSendMailRequest
      { from: "admin@buzgibi.app"
      , personalization: "admin"
      , subject: "technical hitch"
      , body: "survey " <> show ident <> " cannot be carried out"
      }
    resp <- Request.make apiBuzgibiHost BuzgibiBack.mkForeignApi $ BuzgibiBack.sendEmail email
    let msg = "thank ypu for the request! we are getting to it"
    withError resp $ const $ Async.send $ Async.mkOrdinary msg Async.Success Nothing

render { list: [], constants } =
  HH.div_
    [ HH.div_ [ HH.text "you haven't the history to be shown" ]
    , HH.div_
        [ HH.a
            [ css "nav-link"
            , HPExt.style "font-size: 20px"
            , safeHref Route.UserSurvey
            ]
            [ HH.text $ fromMaybe undefined $ Map.lookup "makeSurvey" constants ]
        ]
    ]
render { list, total, perpage, constants, currPage } =
  HH.div
    [ css "history-item-container" ]
    [ HH.table_
        [ HH.thead_
            [ HH.th [ HPExt.scope ScopeCol ] [ HH.text $ fromMaybe "..." (Map.lookup "title" constants) ]
            , HH.th [ HPExt.scope ScopeCol ] [ HH.text $ fromMaybe "..." (Map.lookup "time" constants) ]
            , HH.th [ HPExt.scope ScopeCol ] [ HH.text $ fromMaybe "..." (Map.lookup "status" constants) ]
            , HH.th [ HPExt.scope ScopeCol ] [ HH.text $ fromMaybe "..." (Map.lookup "report" constants) ]
            ]
        , HH.tbody_
            ( list <#> \{ surveyident, reportident, name, timestamp, status, voice } ->
                HH.tr_
                  [ HH.td [ HPExt.dataLabel "title" ]
                      ( HH.text (if length name > 20 then take 20 name else name) :
                          if status == "draft" then
                            [ HH.a
                                [ css "nav-link"
                                , safeHref (Route.EditSurvey surveyident)
                                , HE.onClick (Edit surveyident (unsafeFromForeign voice))
                                ]
                                (HH.text "edit" : if isUndefined voice then [ HH.div [ css "voice-lock-loader " ] [] ] else [])
                            , HH.a
                                [ css "nav-link"
                                , HE.onClick (Submit surveyident (isUndefined voice))
                                ]
                                [ HH.text "submit" ]
                            ]
                          else []
                      )
                  , HH.td [ HPExt.dataLabel "time" ] [ HH.text timestamp ]
                  , HH.td [ HPExt.dataLabel "status" ]
                      [ if status == "technicalFailure" then
                          HH.a [ css "nav-link", HE.onClick (TechnicalHitch surveyident) ]
                            [ HH.span [ HPExt.style "color:red" ] [ HH.text $ fromMaybe "..." (Map.lookup status constants) ]
                            , HH.span [ css "toolip-tech-failure" ] [ HH.text "let us know about this issue" ]
                            ]
                        else HH.text $ fromMaybe "..." (Map.lookup status constants)
                      ]
                  , HH.td [ HPExt.dataLabel "report" ] $
                      if isUndefined reportident then [ HH.div_ [ HH.text "-" ] ]
                      else
                        [ HH.form [ HE.onSubmit $ Download ((unsafeFromForeign reportident) :: Int) name ]
                            [ HH.input
                                [ HPExt.style "cursor: pointer"
                                , HPExt.type_ HPExt.InputSubmit
                                , HPExt.value $ if length name > 10 then take 10 name <> "..." else name
                                ]
                            ]
                        ]
                  ]
            )
        ]
    , HH.div [ HPExt.style "margin-top: 10px" ] [ HH.slot_ Pagination.proxy unit Pagination.component { total: total, perpage: perpage, page: currPage } ]
    , HH.div_
        [ HH.a
            [ css "nav-link"
            , safeHref UserSurvey
            ]
            [ HH.text $ fromMaybe "..." (Map.lookup "makeSurvey" constants) ]
        ]
    ]