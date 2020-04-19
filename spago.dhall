{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "dominion-mystic"
, dependencies =
  [ "assert"
  , "console"
  , "debug"
  , "effect"
  , "formatters"
  , "parsing"
  , "psci-support"
  , "spec"
  , "strings"
  , "stringutils"
  , "transformers"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
