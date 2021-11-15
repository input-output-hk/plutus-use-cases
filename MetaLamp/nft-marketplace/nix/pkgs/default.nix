{ pkgs
, sources
, plutus
, haskell-nix
}:
let
  gitignore-nix = pkgs.callPackage plutus."gitignore.nix" { };

  compiler-nix-name = plutus.plutus-apps.haskell.compiler-nix-name;

  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources haskell-nix;
    inherit compiler-nix-name; # Use the same GHC version as plutus
    inherit (pkgs) libsodium-vrf;
  };

  hlint = plutus.plutus-apps.hlint;

  cabal-install = plutus.plutus-apps.cabal-install;

  fix-stylish-haskell = plutus.plutus-apps.fixStylishHaskell;

  stylish-haskell = plutus.plutus-apps.stylish-haskell;

  haskell-language-server = plutus.plutus-apps.haskell-language-server;

  cardano-repo-tool = plutus.plutus-apps.cardano-repo-tool;
in
{
  inherit haskell hlint cabal-install stylish-haskell fix-stylish-haskell haskell-language-server;
  inherit cardano-repo-tool;
}