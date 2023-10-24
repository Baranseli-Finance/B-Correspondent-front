module BCorrespondent.Component.Workspace.BalancedBook.Amount ( Query(..), proxy, slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css)
import BCorrespondent.Api.Foreign.Back as Back

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import AppM (AppM)
import Data.Maybe (Maybe (..))
import Web.Event.Event (preventDefault, Event)
import Data.Array (snoc)
import Effect.Aff as Aff
import Data.Traversable (for_)

proxy = Proxy :: _ "workspace_balanced_book_amount"

loc = "BCorrespondent.Component.Workspace.BalancedBook.Amount"

slot n = HH.slot_ proxy n component unit

data Query a = Open (Array Back.ForeignDayOfWeeksHourlyTotalSum) a

data Action = Close Event

type State = 
    { isOpen :: Boolean, 
      amount :: Array Back.ForeignDayOfWeeksHourlyTotalSum,
      fork :: Maybe H.ForkId
    }

component =
  H.mkComponent
    { initialState: 
      const { isOpen: false, amount: [], fork: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval { handleQuery = handleQuery }
    }
    where
      handleQuery 
        :: forall a s . Query a
        -> H.HalogenM State Action s Unit AppM (Maybe a)
      handleQuery (Open [] a) = pure $ Just a 
      handleQuery (Open xs a) = do
        {fork} <- H.get
        for_ fork H.kill 
        ident <- H.fork $ do
            H.liftAff $ Aff.delay $ Aff.Milliseconds 3000.0
            H.modify_ _ { isOpen = false, amount = [], fork = Nothing }
        map (const (Just a)) $ H.modify_ _ { isOpen = true, amount = xs, fork = Just ident }


render {isOpen, amount: xs} =
  HH.div 
  [css "amount-modal-container", 
   HPExt.style display
  ] $
  xs <#> \{amount, currency} -> 
    HH.div 
    [HPExt.style "padding-top:10px"] 
    [HH.text $ show (amount) <> " " <> show (Back.decodeCurrency currency) ]
  where display = if isOpen then "display:block" else "display:none"
