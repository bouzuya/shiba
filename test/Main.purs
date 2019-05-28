module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.Unit.Main as TestUnitMain
import Test.YearWeekRange as YearWeekRange

main :: Effect Unit
main = TestUnitMain.runTest do
  YearWeekRange.tests
