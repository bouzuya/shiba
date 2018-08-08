module Main
  ( main ) where

import Bouzuya.DateTime (Time(..), exactDateFromWeekOfYear, weekOfYear, year)
import Data.Array (intercalate)
import Data.Array as Array
import Data.DateTime (DateTime(..))
import Data.Maybe (fromJust, maybe)
import DateTimeFormat as DateTimeFormat
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Now (nowDate)
import GitHub (fetchCommit, fetchCommits, fetchRepos, fetchTags)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, bottom, map, pure, top, (&&), (<=), (<>))

main :: Effect Unit
main = launchAff_ do
  let user = "bouzuya"
  reposMaybe <- fetchRepos user
  repos <- maybe (throwError (error "error")) pure reposMaybe
  today <- liftEffect nowDate
  let
    y = year today
    woy = weekOfYear today
    fd = unsafePartial (fromJust (exactDateFromWeekOfYear y woy bottom))
    ld = unsafePartial (fromJust (exactDateFromWeekOfYear y woy top))
    toDateTime d = DateTime d (Time bottom bottom bottom bottom)
    fdt = toDateTime fd
    ldt = toDateTime ld
    filter = Array.filter (\a -> fdt <= a.pushedAt && a.pushedAt <= ldt)
    filtered = filter repos
    repoMaybe = Array.head repos
  repo <- maybe (throwError (error "error")) pure repoMaybe
  commitsMaybe <- fetchCommits repo fdt ldt
  _ <- liftEffect (logShow commitsMaybe)
  tagsMaybe <- fetchTags repo
  tags <- maybe (throwError (error "error")) pure tagsMaybe
  let tagMaybe = Array.head tags
  tag <- maybe (throwError (error "error")) pure tagMaybe
  commitMaybe <- fetchCommit repo tag.commit.sha
  _ <- liftEffect (logShow tagsMaybe)
  _ <- liftEffect (logShow commitMaybe)
  let
    dateLine =
      ( (DateTimeFormat.format DateTimeFormat.iso8601DateFormat fdt)
      <> "/"
      <> (DateTimeFormat.format DateTimeFormat.iso8601DateFormat ldt)
      )
  liftEffect
    (log
      (intercalate
        "\n"
        ([dateLine, ""] <> (map (\s -> "- [" <> s <> "][]") (map _.fullName filtered)))
      )
    )
