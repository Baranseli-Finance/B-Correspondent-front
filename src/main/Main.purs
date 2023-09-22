module Main (main) where

import Prelude (Unit, bind, discard, flip, map, pure, show, unit, void, when, ($), (/=), (<<<), (<>), (>>=), (==), (*>))

import Buzgibi.Data.Route (routeCodec)
import Buzgibi.Component.Root as Root
import Buzgibi.Data.Config as Cfg
import Buzgibi.Api.Foreign.BuzgibiBack (getShaCSSCommit, getShaCommit, getCookiesInit, getJwtStatus, JWTStatus(..), JWTToken(..))
import Buzgibi.Component.Lang.Data (Lang(..))
import Buzgibi.Component.AppInitFailure as AppInitFailure
import Buzgibi.Data.Config
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import AppM as AppM
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)
import Control.Monad.Error.Class (catchError, throwError)
import Buzgibi.Web.Platform (getPlatform)
import Data.Function.Uncurried (runFn1)
import Web.HTML.Navigator (userAgent)
import Web.HTML.Window (navigator, document, localStorage)
import Web.HTML (window)
import Store (initAppStore, User)
import Store.Types (readPlatform, LogLevel(Dev))
import Effect.Exception as Excep
import Data.Either (Either(..))
import Effect.AVar (new, empty) as Async
import Effect.Console (infoShow, logShow)
import Web.DOM.Document (getElementsByTagName, createElement)
import Web.DOM.HTMLCollection (item)
import Web.DOM.Element (setAttribute)
import Web.DOM.Node (appendChild)
import Data.Traversable (for)
import Web.HTML.HTMLDocument (toDocument, toNode)
import Web.DOM.Internal.Types (Element)
import Unsafe.Coerce (unsafeCoerce)
import Cache as Cache
import Data.Foldable (for_)
import Concurrent.Channel (newChannel) as Async
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Core (stringifyWithIndent)
import Web.Storage.Storage (getItem, removeItem)
import Crypto.Jwt as Jwt
import Effect.Ref as Ref

main :: Cfg.Config -> Effect Unit
main cfg = do

  ua <- window >>= navigator >>= userAgent
  _platform <- map readPlatform $ runFn1 getPlatform ua

  HA.runHalogenAff do
    -- To run our Halogen app, we'll need two things:
    -- 1. An HTML element for Halogen to control
    -- 2. A component to mount at that HTML element, along with any arguments it requires

    -- First, we'll get a reference to the HTML element we want Halogen to attach to. Let's get a
    -- reference to the <body> tag as soon as it exists.
    body <- HA.awaitBody

    -- try getting an User's JWTToken
    jwt <- H.liftEffect $ getJWTfromStorage

    -- request the backend to send initial values (such as static content) required to get the app running
    initResp <- initAppStore (_.apiBuzgibiHost (getVal cfg)) $ map JWTToken jwt
    case initResp of
      Left err -> void $ runUI AppInitFailure.component { error: err } body
      Right init -> do

        -- I am sick to the back teeth of changing css hash manualy
        -- let's make the process a bit self-generating
        for_ (_.cssFiles (getVal cfg)) $ H.liftEffect <<< setCssLink (getShaCSSCommit init) (_.cssLink (getVal cfg))

        langVar <- H.liftEffect $ Async.new Eng

        async <- H.liftEffect $ Async.newChannel

        telVar <- H.liftEffect $ Async.newChannel

        logLevel <- H.liftEffect $ withMaybe $ BuzgibiBack.getLogLevel init

        platform <- H.liftEffect $ withMaybe _platform

        isLogoutVar <- H.liftEffect Async.empty

        paginationVar <- H.liftEffect $ Ref.new (Just 1)

        editSurvey <- H.liftEffect Async.empty

        wsVar <- H.liftEffect Async.empty

        when (logLevel == Dev)
          $ H.liftEffect
          $ do
              infoShow $ "init --> " <> show init
              infoShow $ "cfg --> " <> stringifyWithIndent 4 (encodeJson cfg)

        user <- H.liftEffect $ getCurrentUser jwt $ getJwtStatus init

        -- We now have the three pieces of information necessary to configure our app. Let's create
        -- a record that matches the `Store` type our application requires by filling in these three
        -- fields. If our environment type ever changes, we'll get a compiler error here.
        let
          initialStore =
            { config:
                setShaCommit (getShaCommit init)
                  $ setIsCaptcha (fromMaybe false (BuzgibiBack.getIsCaptcha init))
                  $
                    setToTelegram (fromMaybe false (BuzgibiBack.getToTelegram init)) cfg
            , error: Nothing
            , platform: platform
            , init: init
            , cache: Cache.init
            , async: async
            , cookies: getCookiesInit init
            , langVar: langVar
            , telegramVar: telVar
            , logLevel: logLevel
            , user: user
            , isLogoutVar: isLogoutVar
            , paginationVar: paginationVar
            , isTest: fromMaybe false (BuzgibiBack.getIsTest init)
            , editSurvey: editSurvey
            , wsVar: wsVar
            }

        -- With our app environment ready to go, we can prepare the router to run as our root component.
        --
        -- But wait! Our router is configured to run in a monad that supports all our capabilities like
        -- navigation, API requests, and logging. More concretely, we'll run the application in our
        -- custom `AppM` monad, which supports all these.
        --
        -- But Halogen only knows how to run components in the `Aff` (asynchronous effects) monad. `Aff`
        -- has no idea how to interpret our capabilities. We need a way to change our router component so
        -- that it runs in `Aff` instead of `AppM`. We can do that with `runAppM`:
        rootComponent <- AppM.runAppM initialStore Root.component

        -- Now we have the two things we need to run a Halogen application: a reference to an HTML element
        -- and the component to run there.
        --
        -- To run a Halogen application, use the `runUI` function. It accepts the component to run, arguments
        -- to provide to the component (in our case, the landing page), and the reference to an HTML element.
        -- It will start the Halogen application and return a record with two fields:
        --
        -- `query`, which lets us send queries down to the root component
        -- `subscribe`, which lets us listen and react to messages output by the root component
        --
        -- Note: Since our root component is our router, the "queries" and "messages" above refer to the
        -- `Query` and `Message` types defined in the `Conduit.Router` module. Only those queries and
        -- messages can be used, or else you'll get a compiler error.
        halogenIO <- runUI rootComponent unit body

        -- Fantastic! Our app is running and we're almost done. All that's left is to notify the router
        -- any time the location changes in the URL.
        --
        -- We're using hash-based routing, so we'll use the `matchesWith` function from `Routing.Hash` to
        -- listen for changes in the hash and parse the result (using our routing codec, `routeCodec`,
        -- along with the `parse` function from `Routing.Duplex`). Any time we parse a new location we'll
        -- trigger a `Navigate` query in the router.
        --
        -- If you feel confused by what's going on here, I'd recommend the `purescript-routing` and
        -- `purescript-routing-duplex` guides:
        --
        -- https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md
        -- https://github.com/natefaubion/purescript-routing-duplex/blob/v0.2.0/README.md
        void $ liftEffect $ matchesWith (parse routeCodec) \from to ->
          when (from /= Just to) $ launchAff_
            $ flip catchError (liftEffect <<< logShow)
            $ void
            $ halogenIO.query
            $ H.mkTell
            $ Root.Navigate to

-- var head  = document.getElementsByTagName('head')[0];
-- var link  = document.createElement('link');
-- link.id   = cssId;
-- link.rel  = 'stylesheet';
-- link.type = 'text/css';
-- link.href = 'http://website.example/css/stylesheet.css';
-- link.media = 'all';
-- head.appendChild(link);
setCssLink :: String -> String -> String -> Effect Unit
setCssLink sha mkHref file = do
  win <- window
  doc <- map toDocument $ document win
  xs <- "head" `getElementsByTagName` doc
  headm <- 0 `item` xs
  res <- for headm \(head :: Element) -> do
    link :: Element <- "link" `createElement` doc
    setAttribute "rel" "stylesheet" link
    setAttribute "type" "text/css" link
    let href = mkHref <> sha <> "/" <> file <> ".css"
    setAttribute "href" href link
    appendChild (toNode (unsafeCoerce link)) (toNode (unsafeCoerce head))
    pure $ Just unit

  when (isNothing res) $ throwError $ Excep.error "cannot append link to head"

withMaybe :: forall a. Maybe a -> Effect a
withMaybe Nothing = throwError $ Excep.error "value has been resolved into nothing"
withMaybe (Just a) = pure a

getJWTfromStorage :: Effect (Maybe String)
getJWTfromStorage = window >>= localStorage >>= getItem "buzgibi_jwt"

getCurrentUser :: Maybe String -> Maybe JWTStatus -> Effect (Maybe User)
getCurrentUser jwt (Just Valid) =
  for jwt \token -> do
    user <- Jwt.parse token
    pure $ { jwtUser: user, token: JWTToken token }
getCurrentUser _ (Just Invalid) = (window >>= localStorage >>= removeItem "buzgibi_jwt") *> pure Nothing
getCurrentUser _ (Just Skip) = pure Nothing
getCurrentUser _ _ = throwError $ Excep.error $ "value has been resolved into nothing"