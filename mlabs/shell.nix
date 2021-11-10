{ sourcesFile ? ./nix/sources.json, system ? builtins.currentSystem
, sources ? import ./nix/sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, plutus-apps ? import sources.plutus-apps { }
, plutusShell ? import "${sources.plutus}/shell.nix" { }
, deferPluginErrors ? true, doCoverage ? true }@args:

let
  project = import ./default.nix args;
  inherit (plutus) pkgs;

in project.shellFor {
  packages = ps: [ ps.mlabs-plutus-use-cases ];

  tools.cabal = "latest";
  withHoogle = true;

  # Solution in https://github.com/input-output-hk/plutus-starter/commit/3ab180a1c1079c83aeae61d8c6df28e9840aa9cc ?
  # https://github.com/PrivateStorageio/PaymentServer/blob/main/nix/default.nix
  # Define a Nixpkgs overlay with `m` defined such that `ieee` can be added
  # to `additional`, which is needed when `exactDeps = true;`.

  # exactDeps = true;

  /* Is this needed?

     inputsFrom = [ plutusShell ];

     additional = ps: with ps; [
       playground-common
       plutus-contract
       plutus-core
       plutus-ledger-api
       plutus-tx
       plutus-tx-plugin
       plutus-use-cases
       prettyprinter-configurable
     ];
  */

  nativeBuildInputs = with pkgs;
    [
      # Haskell Tools
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

      pkg-config libsodium-vrf
    ] ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R systemdMinimal ]);
}
