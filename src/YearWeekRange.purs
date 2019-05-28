module YearWeekRange
  ( YearWeekRange
  , between
  , fromDateTime
  , toDateTime
  , toString
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.Date as DateFormatter
import Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Bouzuya.DateTime.TimeZoneOffset as TimeZoneOffset
import Bouzuya.DateTime.WeekDate as WeekDate
import Bouzuya.DateTime.WeekDate.Interval.YearWeek as YearWeek
import Data.DateTime (DateTime(..), Time)
import Data.DateTime as DateTime
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Time.Duration as Duration
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe
import Prelude as Ord

data YearWeekRange = YearWeekRange DateTime DateTime

between :: YearWeekRange -> DateTime -> Boolean
between (YearWeekRange f l) d = Ord.between f l d

-- | 指定した日を含む週の範囲を返す
fromDateTime :: DateTime -> Maybe YearWeekRange
fromDateTime nowInUTC = do
  let tzo = jst
  nowInLocal <- utcToLocal tzo nowInUTC
  let
    today = DateTime.date nowInLocal
    yearWeek = YearWeek.fromWeekDate (WeekDate.fromDate today)
  fwd <- YearWeek.firstWeekDate yearWeek
  lwd <- YearWeek.lastWeekDate yearWeek
  let
    fdt = DateTime (WeekDate.toDate fwd) (bottom :: Time)
    ldt = DateTime (WeekDate.toDate lwd) (top :: Time)
  fdtwzInUTC <- localToUTC tzo fdt
  ldtwzInUTC <- localToUTC tzo ldt
  pure (YearWeekRange fdtwzInUTC ldtwzInUTC)

toDateTime :: YearWeekRange -> Tuple DateTime DateTime
toDateTime (YearWeekRange f l) = Tuple.Tuple f l

toString :: YearWeekRange -> String
toString (YearWeekRange f l) =
  let
    tzo = jst
    f' = Unsafe.unsafePartial (Maybe.fromJust (utcToLocal tzo f))
    l' = Unsafe.unsafePartial (Maybe.fromJust (utcToLocal tzo l))
  in
    (DateFormatter.toString (DateTime.date f'))
    <> "/"
    <> (DateFormatter.toString (DateTime.date l'))

-- private

jst :: TimeZoneOffset
jst =
  Unsafe.unsafePartial
    (Maybe.fromJust (TimeZoneOffset.fromDuration (Duration.Hours (-9.0))))

localToUTC :: TimeZoneOffset -> DateTime -> Maybe DateTime
localToUTC tzo dt =
  map OffsetDateTime.toUTCDateTime (OffsetDateTime.fromLocalDateTime tzo dt)

utcToLocal :: TimeZoneOffset -> DateTime -> Maybe DateTime
utcToLocal tzo dt =
  map OffsetDateTime.toLocalDateTime (OffsetDateTime.fromUTCDateTime tzo dt)
