{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }, deferPluginErrors ? true
, doCoverage ? false }:
let inherit (plutus) pkgs;
in pkgs.haskell-nix.cabalProject rec {
  name = "mlabs-plutus-use-cases";

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "mlabs-plutus-use-cases";
    src = ../..;
    subDir = "mlabs";
  };

  # Plutus uses a patched GHC. And so shall we.
  compiler-nix-name = "ghc810420210212";

  # -- Materialization
  # See https://input-output-hk.github.io/haskell.nix/tutorials/materialization/:
  # Update using:
  #   nix-build default.nix 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash
  # plan-sha256 = "0000000000000000000000000000000000000000000000000000";
  # materialized = ./materialization/mlabs-plutus-use-cases.materialized;

  modules = [{
    packages = {
      eventful-sql-common.doHaddock = false;
      eventful-sql-common.ghcOptions = [''
        -XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
                -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses''];

      plutus-use-cases.doHaddock = deferPluginErrors;
      plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

      plutus-contract.doHaddock = deferPluginErrors;
      plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

      plutus-ledger.doHaddock = deferPluginErrors;
      plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

      cardano-crypto-praos.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      cardano-crypto-class.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

      # This allows us to generate .tix coverage files, which could be useful?
      "${src.name}".components.library.doCoverage = doCoverage;
    };
  }];

  # Using this allows us to leave these nix-specific hashes _out_ of cabal.project
  # Normally, they'd be placed under the `source-repository-package` section as a comment like so:
  # `--sha256: ...`
  sha256map = {
    # Enforce we are using the same hash as niv has
    # i.e. this will now fail to nix-build if you bump it but don't bump the `cabal.project`.
    "https://github.com/input-output-hk/plutus.git"."${sources.plutus.rev}" =
      sources.plutus.sha256;
    "https://github.com/input-output-hk/plutus-apps.git"."${sources.plutus-apps.rev}" =
      sources.plutus-apps.sha256;
    "https://github.com/Quid2/flat.git"."${sources.flat.rev}" =
      sources.flat.sha256;
    "https://github.com/input-output-hk/purescript-bridge.git"."${sources.purescript-bridge.rev}" =
      sources.purescript-bridge.sha256;
    "https://github.com/input-output-hk/servant-purescript.git"."${sources.servant-purescript.rev}" =
      sources.servant-purescript.sha256;
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
    "https://github.com/input-output-hk/cardano-addresses"."${sources.cardano-addresses.rev}" =
      sources.cardano-addresses.sha256;
    "https://github.com/j-mueller/cardano-wallet"."${sources.cardano-wallet.rev}" =
      sources.cardano-wallet.sha256;
    "https://github.com/input-output-hk/goblins"."${sources.goblins.rev}" =
      sources.goblins.sha256;
    "https://github.com/input-output-hk/iohk-monitoring-framework"."${sources.iohk-monitoring-framework.rev}" =
      sources.iohk-monitoring-framework.sha256;
    "https://github.com/input-output-hk/ouroboros-network"."${sources.ouroboros-network.rev}" =
      sources.ouroboros-network.sha256;
    "https://github.com/input-output-hk/Win32-network"."${sources.Win32-network.rev}" =
      sources.Win32-network.sha256;
    "https://github.com/input-output-hk/optparse-applicative"."${sources.optparse-applicative.rev}" =
      sources.optparse-applicative.sha256;
    "https://github.com/Liqwid-Labs/plutus-extra.git"."${sources.plutus-extra.rev}" =
      sources.plutus-extra.sha256;
  };
}
