{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "shiba"
, dependencies =
    [ "bouzuya-datetime"
    , "bouzuya-datetime-formatter"
    , "bouzuya-http-client"
    , "debug"
    , "formatters"
    , "now"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
