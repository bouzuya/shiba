module GitHub
  ( Commit
  , Repo
  , Tag
  , fetchCommit
  , fetchRepos
  , fetchTags
  ) where

import Data.Argonaut (Json, jsonParser)
import Data.Argonaut as Json
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Options ((:=))
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import DateTimeFormat as DateTimeFormat
import Effect.Aff (Aff)
import Fetch (fetch)
import Fetch.Options (defaults, method, url)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Prelude (bind, compose, const, join, map, pure, (<>))

type Commit =
  { authorDate :: DateTime }

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

ownerAndRepo :: Repo -> { owner :: String, repo :: String }
ownerAndRepo { fullName } = unsafePartial fromJust do
  s <- pure (String.split (Pattern "/") fullName)
  owner <- Array.head s
  repo <- join (map Array.head (Array.tail s))
  pure { owner, repo }

fetchCommit :: Repo -> String -> Aff (Maybe Commit)
fetchCommit r sha =
  let { owner, repo } = ownerAndRepo r
  in map (compose join (map parseCommit)) (fetchCommit' owner repo sha)

fetchCommit' :: String -> String -> String -> Aff (Maybe String)
fetchCommit' owner repo sha = do
  let baseUrl = "https://api.github.com"
  let path = "/repos/" <> owner <> "/" <> repo <> "/commits/" <> sha
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := (baseUrl <> path)
    )
  pure response.body

parseCommit :: String -> Maybe Commit
parseCommit responseBody =
  let
    maybeFromEither :: forall a b. Either a b -> Maybe b
    maybeFromEither = either (const Nothing) Just
    toJson :: String -> Maybe Json
    toJson = compose maybeFromEither jsonParser
    toRecord :: Json -> Maybe Commit
    toRecord json = do
      o <- Json.toObject json
      commit <- bind (Object.lookup "commit" o) Json.toObject
      author <- bind (Object.lookup "author" commit) Json.toObject
      authorDateString <- bind (Object.lookup "date" author) Json.toString
      authorDate <-
        either
          (const Nothing)
          Just
          (DateTimeFormat.parse
            DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds
            authorDateString)
      pure { authorDate }
  in
    bind (toJson responseBody) toRecord

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
