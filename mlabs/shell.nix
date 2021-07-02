with import ./nix { };
(plutus.plutus.haskell.project.shellFor (pab.env_variables // {

  # Select packages who's dependencies should be added to the shell env
  packages = ps: [ ];

  # Select packages which should be added to the shell env, with their dependencies
  # Should try and get the extra cardano dependencies in here...
  additional = ps:
    with ps; [
      plutus-pab
      plutus-tx
      plutus-tx-plugin
      plutus-contract
      plutus-ledger-api
      pab.plutus_ledger_with_docs
      plutus-core
      playground-common
      prettyprinter-configurable
      plutus-use-cases
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
      plutus.plutus.haskell-language-server

      # Pab
      pab.plutus_pab_client

      # Example contracts
      plutus.plutus-currency
      plutus.plutus-atomic-swap

    ] ++ (builtins.attrValues pab.plutus_pab_exes);

  buildInputs = [ plutus.pkgs.zlib ];

}))
