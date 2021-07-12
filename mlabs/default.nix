{ sourcesFile ? ./nix/sources.json
, system ? builtins.currentSystem
, sources ? import ./nix/sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, deferPluginErrors ? true
, doCoverage ? true }:
let
  project = import ./nix/haskell.nix {
    inherit sourcesFile system sources plutus deferPluginErrors doCoverage;
  };
in project
