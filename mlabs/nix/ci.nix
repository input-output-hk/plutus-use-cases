{ sourcesFile ? ./sources.json
, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, deferPluginErrors ? true
, doCoverage ? true }:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus deferPluginErrors doCoverage;
  };
in rec {
  # These will be built by the CI.
  inherit (project) projectCoverageReport;
  inherit (project.mlabs-plutus-use-cases.components) library;
  inherit (project.mlabs-plutus-use-cases.components.exes) lendex-demo nft-demo mlabs-plutus-use-cases;
  inherit (project.mlabs-plutus-use-cases.components.tests) mlabs-plutus-use-cases-tests;

  # This will run the tests within this build and produce the test logs as output
  check = plutus.pkgs.runCommand "run-tests" { } ''
    ${mlabs-plutus-use-cases-tests}/bin/mlabs-plutus-use-cases-tests > $out
  '';

}
