{
  description = "mlabs-plutus-use-cases";

  inputs = {
    haskell-nix.url = "github:L-as/haskell.nix?ref=master";

    nixpkgs.follows = "haskell-nix/nixpkgs-2105";

    iohk-nix.url = "github:input-output-hk/iohk-nix";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/d2f86caa085402a953920c6714a0de6a50b655ec";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/4ea7e2d927c9a7f78ddc69738409a5827ab66b98";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/07397f0e50da97eaa0575d93bee7ac4b2b2576ec";
      flake = false;
    };
    cardano-ledger-specs = {
      url =
        "github:input-output-hk/cardano-ledger-specs/bf008ce028751cae9fb0b53c3bef20f07c06e333";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/b6ca519f97a0e795611a63174687e6bb70c9f752";
      flake = false;
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/fd773f7a58412131512b9f694ab95653ac430852";
      flake = false;
    };
    cardano-wallet = {
      url =
        "github:j-mueller/cardano-wallet/6be73ab852c0592713dfe78218856d4a8a0ee69e";
      flake = false;
    };
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/46f994e216a1f8b36fe4669b47b2a7011b0e153c";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/1f4973f36f689d6da75b5d351fb124d66ef1057d";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/2721c59fd2302b75c4138456c29fd5b509e8340a";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:input-output-hk/plutus-apps/21b592b1ea4bc727c1d486432e8aa8388d9e706c";
      flake = false;
    };
    plutus-extra = {
      url =
        "github:t4ccer/plutus-extra/80b48c148b49deb68c436e8bbdf289633c042b06";
      flake = false;
    };
    plutus-tx-spooky = {
      url =
        "gitlab:fresheyeball/plutus-tx-spooky/0c409907fa5b6aee4a2f2d18f871b850a8547fdf";
      flake = false;
    };
    purescript-bridge = {
      url =
        "github:input-output-hk/purescript-bridge/366fc70b341e2633f3ad0158a577d52e1cd2b138";
      flake = false;
    };
    servant-purescript = {
      url =
        "github:input-output-hk/servant-purescript/ebea59c7bdfc0338d83fca772b9a57e28560bcde";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , iohk-nix
    , ...
    }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [
            haskell-nix.overlay
            iohk-nix.overlays.crypto
          ];
          inherit (haskell-nix) config;
          inherit system;
        };

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plutus = import inputs.plutus { inherit system; };
          src = ./.;
        in
        import ./nix/haskell.nix { inherit src inputs pkgs system; };

    in
    {
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem
        (system:
          let
            lib = "mlabs-plutus-use-cases:lib:mlabs-plutus-use-cases";
          in
          self.flake.${system}.packages.${lib}
        );

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages;
          } "touch $out"
      );

      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      #
      # Includes all of the packages in the `checks`, otherwise only the
      # test suite would be included
      checks = perSystem (system: self.flake.${system}.checks);
    };
}
