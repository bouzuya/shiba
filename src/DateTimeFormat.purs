module DateTimeFormat
  ( format
  , iso8601DateTimeFormatWithoutMilliseconds
  , parse
  ) where

import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Formatter.DateTime as Formatter
import Data.List as List

format :: Formatter.Formatter -> DateTime -> String
format = Formatter.format

-- ISO 8601 date time format (without milliseconds)
-- e.g. 2018-01-02T03:04:05Z
iso8601DateTimeFormatWithoutMilliseconds :: Formatter.Formatter
iso8601DateTimeFormatWithoutMilliseconds =
  List.fromFoldable
  [ Formatter.YearFull
  , Formatter.Placeholder "-"
  , Formatter.MonthTwoDigits
  , Formatter.Placeholder "-"
  , Formatter.DayOfMonthTwoDigits
  , Formatter.Placeholder "T"
  , Formatter.Hours24
  , Formatter.Placeholder ":"
  , Formatter.MinutesTwoDigits
  , Formatter.Placeholder ":"
  , Formatter.SecondsTwoDigits
  , Formatter.Placeholder "Z"
  ]

parse :: Formatter.Formatter -> String -> Either String DateTime
parse = Formatter.unformat
