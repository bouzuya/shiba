module Main
  ( main ) where

import Bouzuya.DateTime (Time(..), adjust, exactDateFromWeekOfYear, weekOfYear, year)
import Data.Array (intercalate, zip)
import Data.Array as Array
import Data.DateTime (DateTime(..))
import Data.Maybe (fromJust, isJust, maybe)
import Data.Time.Duration (Hours(..))
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
import Prelude (Unit, bind, bottom, eq, map, negate, pure, top, (&&), (<=), (<>), (>))

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
    fd = unsafePartial (fromJust (exactDateFromWeekOfYear y woy bottom)) -- JST
    ld = unsafePartial (fromJust (exactDateFromWeekOfYear y woy top))    -- JST
    toDateTime d = DateTime d (Time bottom bottom bottom bottom)
    fdt = (toDateTime fd)
    fdtwz = unsafePartial (fromJust (adjust (Hours (negate 9.0)) fdt)) -- UTC
    ldt = (toDateTime ld)
    ldtwz = unsafePartial (fromJust (adjust (Hours (negate 9.0)) ldt)) -- UTC
    filter = Array.filter (\a -> fdtwz <= a.updatedAt && a.updatedAt <= ldtwz)
  reposMaybe <- fetchRepos user
  repos <- maybe (throwError (error "repos error")) pure reposMaybe
  let filtered = filter repos
  tags <- traverse (getTags fdtwz ldtwz) filtered
  let
    zipped :: Array (Tuple Repo (Array Tag))
    zipped = zip filtered tags
  let
    dateLine =
      ( (DateTimeFormat.format DateTimeFormat.iso8601DateFormat fdt)
      <> "/"
      <> (DateTimeFormat.format DateTimeFormat.iso8601DateFormat ldt)
      )
    repoLines =
      map
        (\(Tuple r ts) ->
          "- [" <> r.fullName <> "][] ... "
          <>
            if Array.length ts > 0
            then (intercalate "," (map _.name ts))
            else "(none)"
        )
        zipped
  liftEffect (log (intercalate "\n" ([dateLine, ""] <> repoLines)))
