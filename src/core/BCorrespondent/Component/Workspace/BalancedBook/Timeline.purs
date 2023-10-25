module BCorrespondent.Component.Workspace.BalancedBook.Timeline (Output(..), slot) where

import Prelude

import BCorrespondent.Component.HTML.Utils (css, maybeElem)
import BCorrespondent.Capability.LogMessages (logDebug)
import BCorrespondent.Component.Workspace.BalancedBook.History as History

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import System.Time (nowDate)
import Data.Date as D
import Data.String (split)
import Data.String.Pattern (Pattern (..))
import Data.Int (fromString)
import Data.Array (index)
import Data.Foldable (for_)
import Data.Maybe (Maybe (..))

proxy = Proxy :: _ "workspace_balanced_book_timeline"

loc = "BCorrespondent.Component.Workspace.BalancedBook.Timeline"

slot n params = HH.slot proxy n component params

data Output = OutputBack

data Action = Initialize | Back

type State = { date :: String, from :: Int, history :: Maybe History.Date }

component =
  H.mkComponent
    { initialState: \{date, from} -> { date: date, from: from, history: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize 
      }
    }

handleAction Back = H.raise OutputBack
handleAction Initialize = do
  now <- H.liftEffect nowDate
  {date, from} <- H.get
  let dateXs = split (Pattern "-") date <#> fromString
  let dateRecord = do
        y <- join $ index dateXs 2
        m <- join $ index dateXs 1
        d <- join $ index dateXs 0
        pure { year: y, month: m, day: d }
  for_ dateRecord \{year: y , month: m, day: d} -> do
      logDebug $ loc <> " ---> loading history timeline"
      H.modify_ _ { history = Just {year: y , month: m, day: d, hour: from} }

render {history} = 
  HH.div_
  [ 
      HH.div 
      [css "book-container-timeline-back"] 
      [
          HH.span 
          [HPExt.style "cursor:pointer", 
           HE.onClick (const Back)]
          [HH.i [HPExt.style "font-size:40px", css "fa fa-arrow-left fa-2xs" ] [] ]  
      ]
  ,   maybeElem history $ History.slot 0       
  ]
