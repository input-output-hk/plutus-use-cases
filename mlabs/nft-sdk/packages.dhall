let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200502/packages.dhall sha256:1e1ecbf222c709b76cc7e24cf63af3c2089ffd22bbb1e3379dfd3c07a1787694

let overrides = {=}

let additions =
      { servant-support =
          { dependencies =
              [ "console"
              , "prelude"
              , "either"
              , "foldable-traversable"
              , "generics-rep"
              , "effect"
              , "aff"
              , "affjax"
              , "exceptions"
              , "web-xhr"
              , "foreign-generic"
              ]
          , repo =
              "https://github.com/shmish111/purescript-servant-support"
          , version =
              "1805f896560751c48a04d3e29f9c109df850d8d3"
          }
      , concurrent-queues =
          { dependencies =
              [ "aff"
              , "avar"
              ]
          , repo =
              "https://github.com/purescript-contrib/purescript-concurrent-queues.git"
          , version =
              "v1.1.0"
          }
      , foreign-generic =
            upstream.foreign-generic
          // { repo =
                "https://github.com/shmish111/purescript-foreign-generic"
            , version =
                "57692ed7b1bc512bcfddd2c00c27e865e9c21b84"
            }
      , matryoshka =
          { dependencies =
              [ "prelude"
              , "fixed-points"
              , "free"
              , "transformers"
              , "profunctor"
              ]
          , repo =
              "https://github.com/slamdata/purescript-matryoshka.git"
          , version =
              "v0.4.0"
          }
      }

in  upstream // overrides // additions