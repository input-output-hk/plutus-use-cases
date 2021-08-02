{ sourcesFile ? ./sources.json
, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, deferPluginErrors ? true
, doCoverage ? false }:
let inherit (plutus) pkgs;
in pkgs.haskell-nix.cabalProject rec {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "mlabs-plutus-use-cases";
    src = ./..;
  };

  # Plutus uses a patched GHC. And so shall we.
  compiler-nix-name = "ghc810420210212";

  # -- Materialization
  # See https://input-output-hk.github.io/haskell.nix/tutorials/materialization/:
  # Update using:
  #   nix-build default.nix 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash
  # plan-sha256 = "0m56bhk9w3v1zqpig84f9krrp6sqg21w0vxbjiqcxz8n7c39aw54";
  # materialized = ./materialization/mlabs-plutus-use-cases.materialized;

  modules = [{
    packages = {
      eventful-sql-common.doHaddock = false;
      eventful-sql-common.ghcOptions = [        
        "-XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
        -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses"
      ];
      marlowe.doHaddock = deferPluginErrors;
      marlowe.flags.defer-plugin-errors = deferPluginErrors;

      plutus-use-cases.doHaddock = deferPluginErrors;
      plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

      plutus-ledger.doHaddock = deferPluginErrors;
      plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

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

    # input-output-hk/plutus
    "https://github.com/input-output-hk/plutus.git"."${sources.plutus.rev}"
    = sources.plutus.sha256;

    # Quid2/flat
    "https://github.com/Quid2/flat.git"."95e5d7488451e43062ca84d5376b3adcc465f1cd"
    = "06l31x3y93rjpryvlxnpsyq2zyxvb0z6lik6yq2fvh36i5zwvwa3";

    # shmish111/purescript-bridge
    "https://github.com/shmish111/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596"
    = "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";

    # shmish111/servant-purescript
    "https://github.com/shmish111/servant-purescript.git"."a76104490499aa72d40c2790d10e9383e0dbde63"
    = "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";

    # input-output-hk/cardano-base
    "https://github.com/input-output-hk/cardano-base"."4251c0bb6e4f443f00231d28f5f70d42876da055"
    = "02a61ymvx054pcdcgvg5qj9kpybiajg993nr22iqiya196jmgciv";

    # input-output-hk/cardano-crypto
    "https://github.com/input-output-hk/cardano-crypto.git"."f73079303f663e028288f9f4a9e08bcca39a923e"
    = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";

    # input-output-hk/cardano-ledger-specs
    "https://github.com/input-output-hk/cardano-ledger-specs"."097890495cbb0e8b62106bcd090a5721c3f4b36f"
    = "0i3y9n0rsyarvhfqzzzjccqnjgwb9fbmbs6b7vj40afjhimf5hcj";

    # input-output-hk/cardano-prelude
    "https://github.com/input-output-hk/cardano-prelude"."ee4e7b547a991876e6b05ba542f4e62909f4a571"
    = "0dg6ihgrn5mgqp95c4f11l6kh9k3y75lwfqf47hdp554w7wyvaw6";

    # input-output-hk/goblins
    "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba"
    = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";

    # input-output-hk/iohk-monitoring-framework
    "https://github.com/input-output-hk/iohk-monitoring-framework"."a89c38ed5825ba17ca79fddb85651007753d699d"
    = "sha256-jqN12Ll8mrVQL1MBeD+emzGIXT5P+LkenbDflJccl0Q=";

    # input-output-hk/ouroboros-network
    "https://github.com/input-output-hk/ouroboros-network"."6cb9052bde39472a0555d19ade8a42da63d3e904"
    = "0rz4acz15wda6yfc7nls6g94gcwg2an5zibv0irkxk297n76gkmg";
  };
}
