module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Component.Pagination (testCalculateCurrentSegment) 

main :: Effect Unit
main = do
  log "Tests starts ..."
  testCalculateCurrentSegment
  log "Tests ends ..."