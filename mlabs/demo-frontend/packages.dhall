let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210309/packages.dhall sha256:585332a8a11c6420d7287943f81bc2121746cdd352f2cf3f5ecf65053f2afcd3

in  upstream
  with argonaut-generic.repo = "https://github.com/mlabs-haskell/purescript-argonaut-generic.git"
  with argonaut-generic.version = "be02b21bfb79316b3649062d5eb510b4e191ea22"
