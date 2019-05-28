module Test.YearWeekRange
  ( tests
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.DateTime as DateTimeFormatter
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert
import YearWeekRange as YearWeekRange

tests :: TestSuite
tests = TestUnit.suite "YearWeekRange" do
  TestUnit.test "between" do
    let
      unsafeDate s =
        Unsafe.unsafePartial (Maybe.fromJust (DateTimeFormatter.fromString s))
      dt = unsafeDate "2019-01-02T03:04:05" -- 2019-01-02T12:04:05+09:00
      fdt = unsafeDate "2018-12-30T15:00:00"
      out1 = unsafeDate "2018-12-30T14:59:59"
      ldt = unsafeDate "2019-01-06T14:59:59"
      out2 = unsafeDate "2019-01-06T15:00:00"
      -- = 2018-12-31T00:00:00+09:00/2019-01-06T23:59:59+09:00
      -- = 2018-12-30T15:00:00Z/2019-01-06T14:59:59Z
      ywr =
        Unsafe.unsafePartial (Maybe.fromJust (YearWeekRange.fromDateTime dt))
    Assert.equal false (YearWeekRange.between ywr out1)
    Assert.equal true (YearWeekRange.between ywr fdt)
    Assert.equal true (YearWeekRange.between ywr ldt)
    Assert.equal false (YearWeekRange.between ywr out2)

  TestUnit.test "fromDateTime" do
    let
      unsafeDate s =
        Unsafe.unsafePartial (Maybe.fromJust (DateTimeFormatter.fromString s))
      dt = unsafeDate "2019-01-02T03:04:05" -- 2019-01-02T12:04:05+09:00
      -- = 2018-12-31T00:00:00+09:00/2019-01-06T23:59:59+09:00
      -- = 2018-12-30T15:00:00Z/2019-01-06T14:59:59Z
      ywrMaybe = YearWeekRange.fromDateTime dt
    Assert.equal
      (Maybe.Just "2018-12-31/2019-01-06")
      (map YearWeekRange.toString ywrMaybe)
