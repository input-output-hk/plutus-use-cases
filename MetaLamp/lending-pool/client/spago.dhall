{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "plutus-pab-client"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "avar"
  , "bigints"
  , "concurrent-queues"
  , "console"
  , "debug"
  , "effect"
  , "foreign-generic"
  , "halogen"
  , "matryoshka"
  , "newtype"
  , "node-fs"
  , "prelude"
  , "psci-support"
  , "remotedata"
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
