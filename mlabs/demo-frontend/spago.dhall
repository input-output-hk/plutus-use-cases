{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "arrays"
  , "bigints"
  , "console"
  , "datetime"
  , "debug"
  , "either"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "foreign"
  , "halogen"
  , "integers"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "strings"
  , "tuples"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "test/**/*.purs" ]
}
