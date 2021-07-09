{ sourcesFile ? ./sources.json, system ? builtins.currentSystem }: rec {
  sources = import ./sources.nix { inherit sourcesFile system; };
  plutus = import sources.plutus { };
  pkgs = plutus.pkgs;
  pab = import ./pab.nix { inherit plutus; };
}
