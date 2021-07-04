with import ./nix { };
(plutus.plutus.haskell.project.shellFor (pab.env_variables // {

  # Select packages who's dependencies should be added to the shell env
  packages = ps: [ ];

  # Select packages which should be added to the shell env, with their dependencies
  # Should try and get the extra cardano dependencies in here...
  additional = ps:
    with ps; [
      pab.plutus_ledger_with_docs
      playground-common
      plutus-contract
      plutus-core
      plutus-ledger-api
      plutus-pab
      plutus-tx
      plutus-tx-plugin
      plutus-use-cases
      prettyprinter-configurable
    ];

  withHoogle = true;

  # Extra haskell tools (arg passed on to mkDerivation)
  # Using the plutus.pkgs to use nixpkgs version from plutus (nixpkgs-unstable, mostly)
  propagatedBuildInputs = with pkgs;
    [
      # Haskell Tools
      ghc
      ghcid
      git
      haskellPackages.fourmolu
      haskellPackages.record-dot-preprocessor
      haskellPackages.record-hasfield
      nixfmt
      plutus.plutus.haskell-language-server
      plutus.plutus.hlint
      stack

      # Pab
      pab.plutus_pab_client

      # Example contracts
      plutus.plutus-atomic-swap
      plutus.plutus-currency

    ] ++ (builtins.attrValues pab.plutus_pab_exes);

  buildInputs = [ plutus.pkgs.zlib ];

}))
