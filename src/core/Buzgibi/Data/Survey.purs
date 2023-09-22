module Buzgibi.Data.Survey
  ( AssessmentScore(..)
  , Category(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show
import Data.Enum
import Data.Maybe
import Data.Bounded
import Data.Enum.Generic (genericFromEnum, genericToEnum, genericSucc, genericPred, genericCardinality)

data Category
  = CustomerSatisfaction
  | MarketResearch
  | ProductCampaign
  | SocialResearch
  | PoliticalPoll

derive instance Generic Category _
derive instance Eq Category
derive instance Ord Category

instance Show Category where
  show CustomerSatisfaction = "customerSatisfaction"
  show MarketResearch = "marketResearch"
  show ProductCampaign = "productCampaign"
  show SocialResearch = "socialResearch"
  show PoliticalPoll = "politicalPoll"

instance Enum Category where
  succ = genericSucc
  pred = genericPred

instance BoundedEnum Category where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Bounded Category where
  top = PoliticalPoll
  bottom = CustomerSatisfaction

data AssessmentScore = YN | ScaleOf10

derive instance Generic AssessmentScore _
derive instance Eq AssessmentScore
derive instance Ord AssessmentScore

instance Show AssessmentScore where
  show YN = "yn"
  show ScaleOf10 = "scaleOfTen"

instance Enum AssessmentScore where
  succ = genericSucc
  pred = genericPred

instance BoundedEnum AssessmentScore where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Bounded AssessmentScore where
  top = ScaleOf10
  bottom = YN

