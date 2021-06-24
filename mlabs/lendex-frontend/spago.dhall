{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "halogen"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "test/**/*.purs", "common-frontend/src/**/*.purs" ]
}
