module Main
  ( main ) where

import Data.Argonaut (Json, jsonParser)
import Data.Argonaut as Json
import Data.Array as Array
import Data.DateTime (DateTime(..))
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Traversable (traverse)
import DateTimeFormat as DateTimeFormat
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (Unit, bind, compose, const, join, map, pure, (<>))

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
  response <- map (compose join (map parseRepos)) fetchRepos
  _ <- liftEffect (logShow response)
  liftEffect (log "Hello")
