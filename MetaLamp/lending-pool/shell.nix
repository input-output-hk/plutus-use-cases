with import ./nix { };
(plutus.plutus.haskell.project.shellFor ({

  # Select packages who's dependencies should be added to the shell env
  packages = ps: [ ];

  # Select packages which should be added to the shell env, with their dependencies
  # Should try and get the extra cardano dependencies in here...
  additional = ps:
    with ps; [
      freer-extras
      playground-common
      plutus-core
      plutus-contract
      plutus-ledger
      plutus-ledger-api
      plutus-tx
      plutus-tx-plugin
      plutus-pab
      plutus-use-cases
      prettyprinter-configurable
      quickcheck-dynamic
      word-array
    ];

  withHoogle = true;

  # Extra haskell tools (arg passed on to mkDerivation)
  # Using the plutus.pkgs to use nixpkgs version from plutus (nixpkgs-unstable, mostly)
  propagatedBuildInputs = with pkgs;
    [
      # Haskell Tools
      stack
      plutus.plutus.hlint
      haskellPackages.fourmolu
      git
      ghc
      nixfmt

      # Example contracts
      plutus.plutus-currency
      plutus.plutus-atomic-swap

    ];

  buildInputs = [ plutus.pkgs.zlib ];

}))