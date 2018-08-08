module Main
  ( main ) where

import Bouzuya.DateTime (Time(..), exactDateFromWeekOfYear, weekOfYear, year)
import Data.Argonaut (Json, jsonParser)
import Data.Argonaut as Json
import Data.Array (intercalate)
import Data.Array as Array
import Data.DateTime (DateTime(..))
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Options ((:=))
import Data.Traversable (traverse)
import DateTimeFormat as DateTimeFormat
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Now (nowDate)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import GitHub (Repo, fetchRepos)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, bottom, compose, const, join, map, pure, top, (&&), (<=), (<>))

fetchCommits :: String -> DateTime -> DateTime -> Aff (Maybe String)
fetchCommits fullName since until = do
  let
    s = DateTimeFormat.format DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds since
    u = DateTimeFormat.format DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds until
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := ("https://api.github.com/repos/" <> fullName <> "/commits?since=" <> s <> "&until=" <> u <> "&per_page=100")
    )
  pure response.body

fetchTags :: Repo -> Aff (Maybe String)
fetchTags { fullName } = do
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := ("https://api.github.com/repos/" <> fullName <> "/tags?per_page=100")
    )
  pure response.body

main :: Effect Unit
main = launchAff_ do
  reposMaybe <- fetchRepos
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
  commitsMaybe <- fetchCommits repo.fullName fdt ldt
  _ <- liftEffect (logShow commitsMaybe)
  tagsMaybe <- fetchTags repo
  _ <- liftEffect (logShow tagsMaybe)
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
