module Main
  ( main ) where

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Prelude (Unit)

fetchRepos :: Aff (Maybe String)
fetchRepos = do
  response <- fetch
    ( defaults
    <> method := "GET"
    <> url := "https://api.github.com/users/bouzuya/repos?type=owner&sort=pushed&direction=desc&per_page=100"
    )
  pure response.body

main :: Effect Unit
main = log "Hello"
