module Main
  ( main ) where

import Prelude

import Data.Array (intercalate, zip)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Maybe (isJust, maybe)
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now as Now
import GitHub (Tag, Repo, fetchCommits, fetchRepos, fetchTags)
import Partial.Unsafe as Unsafe
import YearWeekRange as YearWeekRange

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
  now <- liftEffect Now.nowDateTime
  let
    user = "bouzuya"
    range =
      Unsafe.unsafePartial (Maybe.fromJust (YearWeekRange.fromDateTime now))
    Tuple.Tuple fdtwz ldtwz = YearWeekRange.toDateTime range
    filter = Array.filter (\a -> YearWeekRange.between range a.pushedAt)
  reposMaybe <- fetchRepos user
  repos <- maybe (throwError (error "repos error")) pure reposMaybe
  let filtered = filter repos
  tags <- traverse (getTags fdtwz ldtwz) filtered
  let
    zipped :: Array (Tuple Repo (Array Tag))
    zipped = zip filtered tags
  let
    dateLine = YearWeekRange.toString range
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
