{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "plutus-pab-client"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut-codecs"
  , "avar"
  , "bigints"
  , "concurrent-queues"
  , "console"
  , "debug"
  , "effect"
  , "foreign-generic"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "js-date"
  , "matryoshka"
  , "newtype"
  , "node-fs"
  , "now"
  , "prelude"
  , "psci-support"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "servant-support"
  , "test-unit"
  , "transformers"
  , "undefinable"
  , "uuid"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "generated/**/*.purs"
  , "plutus-purs/web-common/**/*.purs"
  , "plutus-purs/web-common-plutus/**/*.purs"
  ]
}
