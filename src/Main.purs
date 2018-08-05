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
import Effect.Class.Console (log)
import Effect.Now (nowDate)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, bottom, compose, const, join, map, pure, top, (&&), (<=), (<>))

type Repo =
  { fullName :: String
  , pushedAt :: DateTime
  }

fetchRepos :: Aff (Maybe String)
fetchRepos = do
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := "https://api.github.com/users/bouzuya/repos?type=owner&sort=pushed&direction=desc&per_page=100"
    )
  pure response.body

parseRepos :: String -> Maybe (Array Repo)
parseRepos responseBody =
  let
    maybeFromEither :: forall a b. Either a b -> Maybe b
    maybeFromEither = either (const Nothing) Just
    toJson :: String -> Maybe Json
    toJson = compose maybeFromEither jsonParser
    toRecords :: Json -> Maybe (Array Repo)
    toRecords json = do
      array <- Json.toArray json
      objects <- traverse Json.toObject array
      pure (Array.catMaybes (map toRecord objects))
    toRecord :: Object Json -> Maybe Repo
    toRecord o = do
      fullName <- bind (Object.lookup "full_name" o) Json.toString
      pushedAtString <- bind (Object.lookup "pushed_at" o) Json.toString
      pushedAt <-
        either
          (const Nothing)
          Just
          (DateTimeFormat.parse
            DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds
            pushedAtString)
      pure { fullName, pushedAt }
  in
    bind (toJson responseBody) toRecords

main :: Effect Unit
main = launchAff_ do
  reposMaybe <- map (compose join (map parseRepos)) fetchRepos
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
