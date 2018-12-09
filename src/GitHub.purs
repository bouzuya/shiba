module GitHub
  ( Commit
  , Repo
  , Tag
  , fetchCommit
  , fetchCommits
  , fetchRepos
  , fetchTags
  ) where

import Bouzuya.HTTP.Client (fetch, headers, method, url)
import Bouzuya.HTTP.Method as Method
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (hush)
import Data.Maybe (Maybe, fromJust)
import Data.Options ((:=))
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import DateTimeFormat as DateTimeFormat
import Effect.Aff (Aff)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Prelude (bind, compose, join, map, pure, (<>))
import Simple.JSON as SimpleJSON

type Commit =
  { authorDate :: DateTime
  , sha :: String
  }

type CommitJSON =
  { commit :: { author :: { date :: String } }
  , sha :: String
  }

type Repo =
  { fullName :: String
  , pushedAt :: DateTime
  , updatedAt :: DateTime
  }

type RepoJSON =
  { full_name :: String
  , pushed_at :: String
  , updated_at :: String
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

type TagJSON =
  { commit ::
    { sha :: String
    , url :: String
    }
  , name :: String
  , node_id :: String
  , tarball_url :: String
  , zipball_url :: String
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
  response <-
    fetch
      ( headers := (Object.fromFoldable [(Tuple "User-Agent" "shiba")])
      <> method := Method.GET
      <> url := (baseUrl <> path)
      )
  pure response.body

parseCommit :: String -> Maybe Commit
parseCommit responseBody =
  let
    toJson :: String -> Maybe CommitJSON
    toJson = compose hush SimpleJSON.readJSON
    toRecord json = do
      sha <- pure json.sha
      authorDateString <- pure json.commit.author.date
      authorDate <-
        hush
          (DateTimeFormat.parse
            DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds
            authorDateString)
      pure { authorDate, sha }
  in
    bind (toJson responseBody) toRecord

fetchCommits :: Repo -> DateTime -> DateTime -> Aff (Maybe (Array Commit))
fetchCommits r since until =
  let { owner, repo } = ownerAndRepo r
  in map (compose join (map parseCommits)) (fetchCommits' owner repo since until)

fetchCommits' :: String -> String -> DateTime -> DateTime -> Aff (Maybe String)
fetchCommits' owner repo since until = do
  let
    s = DateTimeFormat.format DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds since
    u = DateTimeFormat.format DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds until
    url' = "https://api.github.com/repos/" <> owner <> "/" <> repo <> "/commits?since=" <> s <> "&until=" <> u <> "&per_page=100"
  response <-
    fetch
      ( headers := (Object.fromFoldable [(Tuple "User-Agent" "shiba")])
      <> method := Method.GET
      <> url := url'
      )
  pure response.body

parseCommits :: String -> Maybe (Array Commit)
parseCommits responseBody =
  let
    toJson :: String -> Maybe (Array CommitJSON)
    toJson = compose hush SimpleJSON.readJSON
    toRecord json = do
      sha <- pure json.sha
      authorDateString <- pure json.commit.author.date
      authorDate <-
        hush
          ( DateTimeFormat.parse
              DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds
              authorDateString
          )
      pure { authorDate, sha }
  in
    bind (toJson responseBody) (traverse toRecord)

fetchRepos :: String -> Aff (Maybe (Array Repo))
fetchRepos user = map (compose join (map parseRepos)) (fetchRepos' user)

fetchRepos' :: String -> Aff (Maybe String)
fetchRepos' user = do
  let url' = "https://api.github.com/users/" <> user <> "/repos?type=owner&sort=pushed&direction=desc&per_page=100"
  response <-
    fetch
      ( headers := (Object.fromFoldable [(Tuple "User-Agent" "shiba")])
      <> method := Method.GET
      <> url := url'
      )
  pure response.body

parseRepos :: String -> Maybe (Array Repo)
parseRepos responseBody =
  let
    toJson :: String -> Maybe (Array RepoJSON)
    toJson = compose hush SimpleJSON.readJSON
    toRecord :: RepoJSON -> Maybe Repo
    toRecord json = do
      fullName <- pure json.full_name
      pushedAtString <- pure json.pushed_at
      pushedAt <-
        hush
          ( DateTimeFormat.parse
              DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds
              pushedAtString
          )
      updatedAtString <- pure json.updated_at
      updatedAt <-
        hush
          ( DateTimeFormat.parse
              DateTimeFormat.iso8601DateTimeFormatWithoutMilliseconds
              updatedAtString
          )
      pure { fullName, pushedAt, updatedAt }
  in
    bind (toJson responseBody) (traverse toRecord)

fetchTags :: Repo -> Aff (Maybe (Array Tag))
fetchTags repo = map (compose join (map parseTags)) (fetchTags' repo)

fetchTags' :: Repo -> Aff (Maybe String)
fetchTags' { fullName } = do
  let url' = "https://api.github.com/repos/" <> fullName <> "/tags?per_page=100"
  response <-
    fetch
      ( headers := (Object.fromFoldable [(Tuple "User-Agent" "shiba")])
      <> method := Method.GET
      <> url := url'
      )
  pure response.body

parseTags :: String -> Maybe (Array Tag)
parseTags responseBody =
  let
    toJson :: String -> Maybe (Array TagJSON)
    toJson = compose hush SimpleJSON.readJSON
    toRecord json = do
      commit <- pure json.commit
      name <- pure json.name
      nodeId <- pure json.node_id
      tarballUrl <- pure json.tarball_url
      zipballUrl <- pure json.zipball_url
      pure { commit, name, nodeId, tarballUrl, zipballUrl }
  in
    bind (toJson responseBody) (traverse toRecord)
