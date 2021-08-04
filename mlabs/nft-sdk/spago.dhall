{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "avar"
  , "bigints"
  , "concurrent-queues"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-generic"
  , "halogen"
  , "matryoshka"
  , "maybe"
  , "newtype"
  , "node-fs"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "remotedata"
  , "servant-support"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "undefinable"
  , "uuid"
  , "web-socket"
  , "web-uievents"
  , "web-xhr"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  , "generated-src/**/*.purs"
  , "plutus-purs/web-common/**/*.purs"
  , "plutus-purs/web-common-plutus/**/*.purs"
  ]
}
