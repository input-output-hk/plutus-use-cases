{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "argonaut-generic"
  , "bigints"
  , "console"
  , "debug"
  , "effect"
  , "foreign"
  , "halogen"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "test/**/*.purs" ]
}
