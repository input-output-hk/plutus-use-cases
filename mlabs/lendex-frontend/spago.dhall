{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "argonaut-generic"
  , "bigints"
  , "console"
  , "effect"
  , "halogen"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "test/**/*.purs", "common-frontend/src/**/*.purs" ]
}
