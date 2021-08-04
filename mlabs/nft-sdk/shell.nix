{ pkgs ? import <nixpkgs> {} }:
let
  sources = import ./nix/sources.nix {};
  easy-ps = import sources.easy-purescript-nix {};
in
pkgs.mkShell {
  buildInputs = builtins.attrValues {
    inherit (pkgs) gnumake nodejs;
    inherit (easy-ps) purs pulp purp psc-package dhall-simple spago; # psa pscid spago2nix purty zephyr;
  };
}