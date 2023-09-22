module Test.Component.Pagination (testCalculateCurrentSegment) where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert (assertEqual)
import Buzgibi.Component.Pagination (calculateCurrentSegment)
import Data.Maybe (Maybe (..))


testCalculateCurrentSegment :: Effect Unit
testCalculateCurrentSegment = do
  log "testCalculateCurrentSegment: input --> 1 23 5, expects ---> Just [1, 2, 3]"
  let xs = calculateCurrentSegment 1 23 5
  assertEqual {actual: xs, expected: Just {xs: [1, 2, 3], next: Just 2 }}

  log "testCalculateCurrentSegment: input --> 3 23 5, expects ---> Just [1, 2, 3]"
  let xs = calculateCurrentSegment 3 23 5
  assertEqual {actual: xs, expected: Just {xs: [1, 2, 3], next: Just 4 }}

  log "testCalculateCurrentSegment: input --> 4 23 5, expects ---> Just [4, 5]"
  let xs = calculateCurrentSegment 4 23 5
  assertEqual {actual: xs, expected: Just {xs: [4, 5], next: Just 5 }}

  log "testCalculateCurrentSegment: input --> 4 23 5, expects ---> Just [4, 5]"
  let xs = calculateCurrentSegment 5 23 5
  assertEqual {actual: xs, expected: Just {xs: [4, 5], next: Nothing }}