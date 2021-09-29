{ sourcesFile ? ./nix/sources.json
, system ? builtins.currentSystem
, sources ? import ./nix/sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, plutusShell ? import "${sources.plutus}/shell.nix" { }
, deferPluginErrors ? true
, doCoverage ? true }@args:

let
  project = import ./default.nix args;
  inherit (plutus) pkgs;
  pab = import ./nix/pab.nix {
    inherit plutus;
  };
in

project.shellFor (pab.env_variables // {

  tools.cabal = "latest";
  withHoogle = true;

  # Doesn't work
  # Solution in https://github.com/input-output-hk/plutus-starter/commit/3ab180a1c1079c83aeae61d8c6df28e9840aa9cc ?
  # exactDeps = true;

  /* Is this needed?

  inputsFrom = [ plutusShell ];

  additional = ps: with ps; [
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
  */

  nativeBuildInputs = with pkgs;
    [
      # Haskell Tools
      # cabal-install
      entr
      ghcid
      git
      haskellPackages.fourmolu
      nixfmt
      plutus.plutus.haskell-language-server
      plutus.plutus.hlint
      stack
      
      # hls doesn't support preprocessors yet so this has to exist in PATH
      haskellPackages.record-dot-preprocessor

      # Graphviz Diagrams for documentation
      graphviz

      ### Pab
      pab.plutus_pab_client
    ]
    ++ builtins.attrValues plutus.plutus-pab.pab-exes;
})
