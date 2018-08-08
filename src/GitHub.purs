module GitHub
  ( Repo
  , Tag
  , fetchRepos
  , fetchTags
  ) where

import Data.Argonaut (Json, jsonParser)
import Data.Argonaut as Json
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Traversable (traverse)
import DateTimeFormat as DateTimeFormat
import Effect.Aff (Aff)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (bind, compose, const, join, map, pure, (<>))

type Repo =
  { fullName :: String
  , pushedAt :: DateTime
  }

type Tag =
  { commit ::
    { sha :: String
    , url :: String
    }
  , name :: String
  , nodeId :: String
  , tarballUrl :: String
  , zipballUrl :: String
  }

fetchRepos :: String -> Aff (Maybe (Array Repo))
fetchRepos user = map (compose join (map parseRepos)) (fetchRepos' user)

fetchRepos' :: String -> Aff (Maybe String)
fetchRepos' user = do
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := ("https://api.github.com/users/" <> user <> "/repos?type=owner&sort=pushed&direction=desc&per_page=100")
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

fetchTags :: Repo -> Aff (Maybe (Array Tag))
fetchTags repo = map (compose join (map parseTags)) (fetchTags' repo)

fetchTags' :: Repo -> Aff (Maybe String)
fetchTags' { fullName } = do
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := ("https://api.github.com/repos/" <> fullName <> "/tags?per_page=100")
    )
  pure response.body

parseTags :: String -> Maybe (Array Tag)
parseTags responseBody =
  let
    maybeFromEither :: forall a b. Either a b -> Maybe b
    maybeFromEither = either (const Nothing) Just
    toJson :: String -> Maybe Json
    toJson = compose maybeFromEither jsonParser
    toRecords :: Json -> Maybe (Array Tag)
    toRecords json = do
      array <- Json.toArray json
      objects <- traverse Json.toObject array
      pure (Array.catMaybes (map toRecord objects))
    toRecord :: Object Json -> Maybe Tag
    toRecord o = do
      name <- bind (Object.lookup "name" o) Json.toString
      commitObject <- bind (Object.lookup "commit" o) Json.toObject
      commit <- do
        sha <- bind (Object.lookup "sha" commitObject) Json.toString
        url <- bind (Object.lookup "url" commitObject) Json.toString
        pure { sha, url }
      tarballUrl <- bind (Object.lookup "tarball_url" o) Json.toString
      zipballUrl <- bind (Object.lookup "zipball_url" o) Json.toString
      nodeId <- bind (Object.lookup "node_id" o) Json.toString
      pure { commit, name, nodeId, tarballUrl, zipballUrl }
  in
    bind (toJson responseBody) toRecords
