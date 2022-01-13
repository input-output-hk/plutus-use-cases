{ src
, pkgs
, plutus
, doCoverage ? false
, deferPluginErrors ? true
, ...
}:

let
  plutusPkgs = plutus.pkgs;

  sources = import ./sources.nix {};
in
pkgs.haskell-nix.cabalProject {
  inherit src;

  name = "mlabs-plutus-use-cases";

  cabalProjectFileName = "cabal.project";

  # Plutus uses a patched GHC. And so shall we.
  compiler-nix-name = "ghc810420210212";

  # -- Materialization
  # See https://input-output-hk.github.io/haskell.nix/tutorials/materialization/:
  # Update using:
  #   nix-build default.nix 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash
  # plan-sha256 = "0000000000000000000000000000000000000000000000000000";
  # materialized = ./materialization/mlabs-plutus-use-cases.materialized;

  shell = {
    # Make sure to keep this list updated after upgrading git dependencies!
    additional = ps: with ps; [
      plutus-extra
      tasty-plutus
      plutus-pretty
      plutus-numeric
      base-deriving-via
      cardano-addresses
      cardano-addresses-cli
      cardano-binary
      cardano-crypto
      cardano-crypto-class
      cardano-crypto-praos
      cardano-crypto-wrapper
      cardano-ledger-alonzo
      cardano-ledger-byron
      cardano-ledger-core
      cardano-ledger-pretty
      cardano-ledger-shelley
      cardano-ledger-shelley-ma
      cardano-prelude
      cardano-slotting
      flat
      freer-extras
      goblins
      measures
      orphans-deriving-via
      playground-common
      plutus-chain-index
      plutus-contract
      plutus-core
      plutus-ledger
      plutus-ledger-api
      plutus-pab
      plutus-playground-server
      plutus-tx
      plutus-tx-plugin
      plutus-tx-spooky
      plutus-use-cases
      prettyprinter-configurable
      quickcheck-dynamic
      Win32-network
      word-array
    ];

    withHoogle = true;

    tools.cabal = "latest";

    exactDeps = true;

    nativeBuildInputs = with pkgs;
      [
        # Haskell Tools
        entr
        ghcid
        git

        # Use plutus for these packages for now, the versions from haskell.nix
        # nixpkgs are too new and require builds
        plutusPkgs.haskellPackages.fourmolu
        plutusPkgs.niv
        plutusPkgs.stack

        plutus.plutus.haskell-language-server
        plutus.plutus.hlint
        jq
        nixfmt

        # hls doesn't support preprocessors yet so this has to exist in PATH
        haskellPackages.record-dot-preprocessor

        # Graphviz Diagrams for documentation
        graphviz
        pkg-config
        plutusPkgs.libsodium-vrf
      ] ++ (
        lib.optionals (!stdenv.isDarwin) [
          rPackages.plotly
          R
          systemdMinimal
        ]
      );
  };


  modules = [
    {
      packages = {
        eventful-sql-common.doHaddock = false;
        eventful-sql-common.ghcOptions = [
          ''
            -XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
                    -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses''
        ];

        plutus-use-cases.doHaddock = deferPluginErrors;
        plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

        plutus-contract.doHaddock = deferPluginErrors;
        plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

        plutus-ledger.doHaddock = deferPluginErrors;
        plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

        cardano-crypto-praos.components.library.pkgconfig =
          plutusPkgs.lib.mkForce [ [ plutusPkgs.libsodium-vrf ] ];
        cardano-crypto-class.components.library.pkgconfig =
          plutusPkgs.lib.mkForce [ [ plutusPkgs.libsodium-vrf ] ];
      };
    }
  ];

  # Using this allows us to leave these nix-specific hashes _out_ of cabal.project
  # Normally, they'd be placed under the `source-repository-package` section as a comment like so:
  # `--sha256: ...`
  sha256map = {
    # Enforce we are using the same hash as niv has
    # i.e. this will now fail to nix-build if you bump it but don't bump the `cabal.project`.

    # `plutus`, `plutus-apps`, & `plutus-extra`
    "https://github.com/input-output-hk/plutus.git"."${sources.plutus.rev}" =
      sources.plutus.sha256;
    "https://github.com/input-output-hk/plutus-apps.git"."${sources.plutus-apps.rev}" =
      sources.plutus-apps.sha256;
    "https://github.com/Liqwid-Labs/plutus-extra.git"."${sources.plutus-extra.rev}" =
      sources.plutus-extra.sha256;

    # `cardano-*`
    "https://github.com/input-output-hk/cardano-addresses"."${sources.cardano-addresses.rev}" =
      sources.cardano-addresses.sha256;
    "https://github.com/input-output-hk/cardano-base"."${sources.cardano-base.rev}" =
      sources.cardano-base.sha256;
    "https://github.com/input-output-hk/cardano-crypto.git"."${sources.cardano-crypto.rev}" =
      sources.cardano-crypto.sha256;
    "https://github.com/input-output-hk/cardano-ledger-specs"."${sources.cardano-ledger-specs.rev}" =
      sources.cardano-ledger-specs.sha256;
    "https://github.com/input-output-hk/cardano-node.git"."${sources.cardano-node.rev}" =
      sources.cardano-node.sha256;
    "https://github.com/input-output-hk/cardano-prelude"."${sources.cardano-prelude.rev}" =
      sources.cardano-prelude.sha256;
    "https://github.com/j-mueller/cardano-wallet"."${sources.cardano-wallet.rev}" =
      sources.cardano-wallet.sha256;

    # other git dependencies
    "https://github.com/Quid2/flat.git"."${sources.flat.rev}" =
      sources.flat.sha256;
    "https://github.com/input-output-hk/goblins"."${sources.goblins.rev}" =
      sources.goblins.sha256;
    "https://github.com/input-output-hk/iohk-monitoring-framework"."${sources.iohk-monitoring-framework.rev}" =
      sources.iohk-monitoring-framework.sha256;
    "https://github.com/input-output-hk/ouroboros-network"."${sources.ouroboros-network.rev}" =
      sources.ouroboros-network.sha256;
    "https://github.com/input-output-hk/optparse-applicative"."${sources.optparse-applicative.rev}" =
      sources.optparse-applicative.sha256;
    "https://github.com/input-output-hk/purescript-bridge.git"."${sources.purescript-bridge.rev}" =
      sources.purescript-bridge.sha256;
    "https://github.com/input-output-hk/servant-purescript.git"."${sources.servant-purescript.rev}" =
      sources.servant-purescript.sha256;
    "https://github.com/input-output-hk/Win32-network"."${sources.Win32-network.rev}" =
      sources.Win32-network.sha256;
    "https://gitlab.com/fresheyeball/plutus-tx-spooky"."${sources.plutus-tx-spooky.rev}" =
      sources.plutus-tx-spooky.sha256;
  };
}
