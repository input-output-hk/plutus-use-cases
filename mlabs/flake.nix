{
  description = "mlabs-plutus-use-cases";

  inputs = {
    haskell-nix.url = "github:L-as/haskell.nix/ac825b91c202947ec59b1a477003564cc018fcec";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    nixpkgs.follows = "haskell-nix/nixpkgs";

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
        "github:input-output-hk/cardano-base/654f5b7c76f7cc57900b4ddc664a82fc3b925fb0";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/bf008ce028751cae9fb0b53c3bef20f07c06e333";
      flake = false;
    };
    cardano-node = {
      url = "github:input-output-hk/cardano-node/4f65fb9a27aa7e3a1873ab4211e412af780a3648";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    cardano-wallet = {
      url =
        "github:j-mueller/cardano-wallet/760140e238a5fbca61d1b286d7a80ece058dc729";
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
        "github:input-output-hk/ouroboros-network/d613de3d872ec8b4a5da0c98afb443f322dc4dab";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/65bad0fd53e432974c3c203b1b1999161b6c2dce";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:t4ccer/plutus-apps/0d3ad307e5875cd5a90df8aa8cea46962eaa5c85";
      flake = false;
    };
    plutus-extra = {
      url =
        "github:Liqwid-Labs/plutus-extra/4722305495c8c4b03ff06debf0f4a041768a5467";
      flake = false;
    };
    plutus-tx-spooky = {
      url =
        "gitlab:fresheyeball/plutus-tx-spooky/0c409907fa5b6aee4a2f2d18f871b850a8547fdf";
      flake = false;
    };
    plutus-simple-model = {
      url =
        "github:t4ccer/plutus-simple-model/48c186f96e3a8a07bceb1a4b39a7dfeacddde42b";
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
    plutip = {
      url = "github:mlabs-haskell/plutip/c2d0ed381cda64bc46dbf68f52cb0d05f76f3a86";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.cardano-node.follows = "cardano-node";
    };
    bot-plutus-interface = {
      follows = "plutip/bot-plutus-interface";
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay iohk-nix.overlays.crypto ];
          inherit (haskell-nix) config;
          inherit system;
        };

      nixpkgsFor' = system: import nixpkgs { inherit system; };

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plutus = import inputs.plutus { inherit system; };
          src = ./.;
        in import ./nix/haskell.nix { inherit src inputs pkgs system; };

    in {
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "mlabs-plutus-use-cases:lib:mlabs-plutus-use-cases";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check" {
          nativeBuildInputs = builtins.attrValues self.checks.${system}
            ++ builtins.attrValues self.flake.${system}.packages
            ++ [ self.flake.${system}.devShell.inputDerivation ];
        } "touch $out");

      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      #
      # Includes all of the packages in the `checks`, otherwise only the
      # test suite would be included
      checks = perSystem (system: self.flake.${system}.checks);
    };
}
