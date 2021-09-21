with import ./nix { };
(plutus.plutus.haskell.project.shellFor (pab.env_variables // {

  # adds the direct shell of the plutus repo
  inherit plutusShell; 

  # Select packages which should be added to the shell env
  packages = ps:
    [
      # criterion 
      # tasty-quickcheck
    ];

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
      cabal-install
      entr
      ghc
      ghcid
      git
      haskellPackages.fourmolu
      nixfmt
      plutus.plutus.haskell-language-server
      plutus.plutus.hlint
      stack

      # Makefile
      gnumake
      
      # hls doesn't support preprocessors yet so this has to exist in PATH
      haskellPackages.record-dot-preprocessor

      # Graphviz Diagrams for documentation
      graphviz

      ### Pab
      pab.plutus_pab_client

    ] ++ (builtins.attrValues pab.plutus_pab_exes);

  buildInputs = (with plutus.pkgs;
    [ zlib pkg-config libsodium systemd ]
    # Dependencies for MacOs
    ++ (lib.optionals (!stdenv.isDarwin) [ R ]));
  
}))