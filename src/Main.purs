module Main
  ( main ) where

import Bouzuya.DateTime (Time(..), exactDateFromWeekOfYear, weekOfYear, year)
import Data.Array (intercalate, zip)
import Data.Array as Array
import Data.DateTime (DateTime(..))
import Data.Maybe (fromJust, isJust, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import DateTimeFormat as DateTimeFormat
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDate)
import GitHub (Tag, Repo, fetchCommits, fetchRepos, fetchTags)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, bottom, eq, map, pure, top, (&&), (<=), (<>))

getTags :: DateTime -> DateTime -> Repo -> Aff (Array Tag)
getTags fdt ldt repo = do
  commitsMaybe <- fetchCommits repo fdt ldt
  commits <- maybe (throwError (error "error")) pure commitsMaybe
  tagsMaybe <- fetchTags repo
  tags <- maybe (throwError (error "error")) pure tagsMaybe
  pure
    (Array.filter
      (\tag -> isJust (Array.find (\commit -> eq tag.commit.sha commit.sha) commits))
      tags
    )

main :: Effect Unit
main = launchAff_ do
  today <- liftEffect nowDate
  let
    user = "bouzuya"
    y = year today
    woy = weekOfYear today
    fd = unsafePartial (fromJust (exactDateFromWeekOfYear y woy bottom))
    ld = unsafePartial (fromJust (exactDateFromWeekOfYear y woy top))
    toDateTime d = DateTime d (Time bottom bottom bottom bottom)
    fdt = toDateTime fd
    ldt = toDateTime ld
    filter = Array.filter (\a -> fdt <= a.pushedAt && a.pushedAt <= ldt)
  reposMaybe <- fetchRepos user
  repos <- maybe (throwError (error "error")) pure reposMaybe
  let filtered = filter repos
  tags <- traverse (getTags fdt ldt) filtered
  let
    zipped :: Array (Tuple Repo (Array Tag))
    zipped = zip filtered tags
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
        ([dateLine, ""] <> (map (\(Tuple r ts) -> "- [" <> r.fullName <> "][] …… " <> (intercalate "," (map _.name ts))) zipped))
      )
    )
